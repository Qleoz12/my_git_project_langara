import pandas as pd
import os

# read files
train = 'train.csv'
test = 'test.csv'
train = pd.read_csv(train, sep=',')

#Explore its general structure:
print(train.info())

#View a sample of the content:
print(train.head(1))

#See the full list of columns:
print(train.columns.tolist())

# See what past columns exist for a variable (e.g. 'bg')
bg = [col for col in train.columns if col.startswith("bg-")]
print(len(bg))
insulin = [col for col in train.columns if col.startswith("insulin-")]
print(len(insulin))

#Group those 72 columns into blocks of 3
# (i.e., 24 blocks of 15 minutes), and then calculate the average per block.
"""
#Column Represents the average glucose over the interval:
#bg_avg_0 From 5:55 hours to 5:40 hours before time
#bg_avg_1 From 5:35 hours to 5:20 hours before time etc..
"""
variables = ["bg", "carbs", "hr", "steps", "cals"]

for var in variables:
    # Usar directamente las columnas, asumiendo que ya están en orden
    var_cols = [col for col in train.columns if col.startswith(f"{var}-")]

    # Agrupar de a 3 columnas y calcular promedio
    for i in range(24):
        cols_3 = var_cols[i*3:(i+1)*3]
        train[f"{var}_avg_{i}"] = train[cols_3].mean(axis=1)

print([col for col in train.columns if "_avg_" in col])

#Subset the DataFrame with only what is necessary

avg_cols = [col for col in train.columns if "_avg_" in col]

# Crear nuevo DataFrame con columnas necesarias
df_clean = train[["p_num", "time"] + avg_cols].copy()
print(df_clean)

# Add the target variable bg+1:00
df_clean["target"] = train["bg+1:00"]
#Make sure bg+1:00 doesn't have too many missing values:
print(df_clean["target"].isna().sum())

#Save the processed dataset
df_clean.to_csv("data/df_clean.csv", index=False)
"""
"""
#verify average of other variables.

# Load the clean dataset
df = pd.read_csv("data/df_clean.csv")
#structure new dataset:
print(df.info())

#Convert time to datetime.time format
df["time"] = pd.to_datetime(df["time"], format="%H:%M:%S")

#We will extract a new column, for example hour, from df["time"].
df["hour"] = df["time"].dt.hour

#minutes within each hour:
df["minute"] = df["time"].dt.minute

#create periods of the day
def get_day_period(hour):
    if 0 <= hour < 6:
        return "night"
    elif 6 <= hour < 12:
        return "morning"
    elif 12 <= hour < 18:
        return "afternoon"
    else:
        return "evening"

df["period"] = df["hour"].apply(get_day_period)
print(df)

#Create the day_id column: We want to identify the day changes for each person (p_num)

# Convertimos la columna 'time' a segundos desde medianoche (para facilitar la comparación)
df["time_seconds"] = df["time"].dt.hour * 3600 + df["time"].dt.minute * 60 + df["time"].dt.second
# Calculamos la diferencia con la fila anterior (por persona)
df["time_diff"] = df.groupby("p_num")["time_seconds"].diff()
# Cuando la diferencia es negativa, quiere decir que se reinició el reloj (nuevo día)
df["new_day"] = (df["time_diff"] < 0).astype(int)
# Acumulamos los cambios para obtener el identificador de día por persona
df["day_id"] = df.groupby("p_num")["new_day"].cumsum()

#verify that day_id is calculated correctly
print(df)

#Save the processed dataset
#df.to_csv("data/df_check.csv", index=False)
"""
#Look every day for patiente and vairable witouth avg
"""
# Check the number of rows
print(f"train shape: {train.shape}")
print(f"df shape: {df.shape}")

train_day = train.copy()
train_day["day_id"] = df['day_id']
train_day["time"] = df["time"]
train_day["hour"] = df["hour"]
#predicted variable
train_day["bg+1:00"] = train["bg+1:00"]
print(train_day)

#count number of nan per day
# Contar valores no nulos por día
non_null_counts = df.groupby('day_id').apply(lambda x: x.notnull().sum().sum())

# Identificar el día con más datos (menos nulos)
best_day = non_null_counts.idxmax()
max_values = non_null_counts.max()

print(f"The day with the fewest null values is the day {best_day} with {max_values} non-null values.")



#Graph the activity curves for each day for a patient
#1. Filter var- columns

import matplotlib.pyplot as plt
variables = ["bg-", "carbs-", "insulin-" "hr-", "steps-", "cals-"]
bg_cols = [col for col in train_day.columns if col.startswith("steps-")]
measurement_type = bg_cols[0].split("-")[0] if bg_cols else "?"

# 2. Choose the day you want to graph
selected_day = 59

# 3. Filter only the rows on the selected day
df_day = train_day[train_day["day_id"] == selected_day]

df_bg = df_day[["p_num", "day_id", "time", "hour"] + bg_cols].copy()
print(f"Datos disponibles para el día {selected_day}: {df_day.shape[0]} filas")
print(f"Columnas de glucosa (bg_cols): {bg_cols}")
# 2. Melt — pasamos de ancho a largo
df_melt = df_bg.melt(
    id_vars=["p_num", "day_id","hour", "time"],
    value_vars=bg_cols,
    var_name="bg_timepoint",
    value_name="bg_value"
)

print(df_melt)

#graph for one patien
import matplotlib.pyplot as plt
import seaborn as sns

#ONE PATIENT
paciente = 'p02'
df_paciente = df_melt[df_melt['p_num'] == paciente]
print(f"Filas para el paciente {paciente}: {df_paciente.shape[0]}")

plt.figure(figsize=(12, 6))
sns.lineplot(data=df_paciente, x='hour', y='bg_value', marker='o')
plt.title(f'Level of {measurement_type} over time - {paciente}')
plt.xlabel('Hour')
plt.ylabel(f'Value of  {measurement_type}')
plt.xticks(rotation=45)
plt.tight_layout()
#plt.show()

#ALL PATIENTS
plt.figure(figsize=(14, 7))
sns.lineplot(data=df_melt, x='hour', y='bg_value', hue='p_num', marker='o')
plt.title('Nivel de glucosa en el tiempo por paciente')
plt.xlabel('Hora')
plt.ylabel('Valor de glucosa (bg_value)')
plt.xticks(rotation=45)
plt.tight_layout()
#plt.show()



# Base variables
variables = ["bg", "insulin", "carbs", "hr", "steps", "cals"]
stats_df = pd.DataFrame()  # This will store the final summary statistics

for var in variables:
    # Find all columns that start with var-
    cols = [col for col in train_day.columns if col.startswith(f"{var}-")]

    # Calculate statistics and add them to the new DataFrame
    stats_df[f"{var}_mean"] = train_day[cols].mean(axis=1)
    stats_df[f"{var}_max"] = train_day[cols].max(axis=1)
    stats_df[f"{var}_sum"] = train_day[cols].sum(axis=1)
    stats_df[f"{var}_std"] = train_day[cols].std(axis=1)

# Print general meaning
print("🧾 Statistics generated for each variable ('bg', 'carbs', 'hr', 'steps', 'cals') over a 6-hour window (72 values, 1 every 5 minutes):")
print("- mean: average value of the variable over that period (overall level)")
print("- max: highest recorded value (peak)")
print("- sum: total sum of all values over 6h (total quantity)")
print("- std: standard deviation, indicates if the variable was stable or highly variable during that period")

# Show the new DataFrame with only the summary statistics
print(stats_df.head())
stats_df["p_num"] = train_day["p_num"]
stats_df["time"] = train_day["time"]
#stats_df.to_csv("data/stats_df.csv", index=False)

#statics per patient

import seaborn as sns
import matplotlib.pyplot as plt

# Solo las columnas numéricas derivadas
numeric_cols = [col for col in stats_df.columns if any(col.startswith(v) for v in ["bg", "insulin", "carbs", "hr", "steps", "cals"]) and col != "p_num"]

# Para cada estadístico
for col in numeric_cols:
    plt.figure(figsize=(10, 5))
    sns.boxplot(data=stats_df, x="p_num", y=col)
    plt.title(f"Distribución de {col} por paciente")
    plt.xticks(rotation=45)
    plt.tight_layout()
    #plt.show()

#correlation between statistics and future glucose

import seaborn as sns
import matplotlib.pyplot as plt

# Unimos estadísticos y variable objetivo en un mismo DataFrame
eda_df = stats_df.copy()
eda_df["bg+1:00"] = train_day["bg+1:00"]  # Agrega la glucosa futura al análisis

# Solo columnas numéricas
corr_cols = [col for col in eda_df.columns if eda_df[col].dtype != 'object']

# Matriz de correlación
corr_matrix = eda_df[corr_cols].corr()

# Visualización
plt.figure(figsize=(12, 10))
sns.heatmap(corr_matrix, annot=True, fmt=".2f", cmap="coolwarm", square=True)
plt.title("Matriz de correlación entre variables estadísticas y bg+1:00")
plt.tight_layout()
#plt.show()

import seaborn as sns
import matplotlib.pyplot as plt

# Filtrar solo las columnas que terminan en "_mean"
mean_cols = [col for col in stats_df.columns if col.endswith("_mean")]

# Añadir la variable objetivo al DataFrame de estadísticos
stats_df_with_target = stats_df[mean_cols].copy()
stats_df_with_target["bg+1:00"] = train_day["bg+1:00"]

# Calcular la matriz de correlación
corr_matrix = stats_df_with_target.corr()

# Visualizar
plt.figure(figsize=(8, 6))
sns.heatmap(corr_matrix, annot=True, cmap="coolwarm", fmt=".2f")
plt.title("Correlation Matrix: Variable Means vs. Target (bg+1:00)")
#plt.show()

#Create a summary per patient

stats_by_patient = stats_df.copy()
stats_by_patient["p_num"] = df["p_num"]
summary_by_patient = stats_by_patient.groupby("p_num").mean()

#Use KMeans to group patients by average behavior:

#This code applies unsupervised clustering with KMeans to group patients based on their 
#numerical characteristics from the summary_by_patient data frame. First, it standardizes 
#the data so that all variables have the same scale (using StandardScaler), then runs KMeans 
#with two groups (clusters) and assigns each patient their respective cluster. Finally, 
#it prints the number of patients in each group and lists the patient IDs belonging to each group.

from sklearn.cluster import KMeans
from sklearn.preprocessing import StandardScaler

X = summary_by_patient.copy()
scaler = StandardScaler()
X_scaled = scaler.fit_transform(X)

kmeans = KMeans(n_clusters=2, random_state=42)
clusters = kmeans.fit_predict(X_scaled)
summary_by_patient["cluster"] = clusters
print(summary_by_patient["cluster"].value_counts())

#wich patient of wich cluster

# Pacientes del cluster 0
pacientes_cluster_0 = summary_by_patient[summary_by_patient["cluster"] == 0].index.tolist()

# Pacientes del cluster 1
pacientes_cluster_1 = summary_by_patient[summary_by_patient["cluster"] == 1].index.tolist()

print("Pacientes en cluster 0:", pacientes_cluster_0)
print("Pacientes en cluster 1:", pacientes_cluster_1)

#cluster per patient

import seaborn as sns
import matplotlib.pyplot as plt

# Definir los grupos de pacientes manualmente
pacientes_cluster_0 = ['p01', 'p02', 'p04', 'p05', 'p06', 'p10', 'p11']
pacientes_cluster_1 = ['p03', 'p12']

# Añadir los identificadores de paciente al DataFrame de estadísticos
stats_df_with_target = stats_df.copy()
stats_df_with_target["p_num"] = df["p_num"]
stats_df_with_target["bg+1:00"] = train_day["bg+1:00"]

# Filtrar solo las columnas que terminan en "_mean"
mean_cols = [col for col in stats_df.columns if col.endswith("_mean")]


# Función para graficar el heatmap por grupo
def plot_corr_for_group(pacientes, cluster_name):
    subset = stats_df_with_target[stats_df_with_target["p_num"].isin(pacientes)]
    data = subset[mean_cols + ["bg+1:00"]].dropna()

    # Verificar que haya datos suficientes
    if data.shape[1] > 1 and not data.empty:
        corr_matrix = data.corr()
        plt.figure(figsize=(8, 6))
        sns.heatmap(corr_matrix, annot=True, cmap="coolwarm", fmt=".2f")
        plt.title(f"Correlation Matrix - Grupo pacientes {cluster_name}")
        plt.tight_layout()
        #plt.show()
    else:
        print(f"No hay suficientes datos para graficar el grupo {cluster_name}.")


# Graficar para cada grupo
plot_corr_for_group(pacientes_cluster_0, "Cluster 0")
plot_corr_for_group(pacientes_cluster_1, "Cluster 1")



#Review distribution of activities

activity_cols = [col for col in train_day.columns if col.startswith("activity-")]
df[activity_cols] = train_day[activity_cols].astype(str)  # Asegurarse que son string

# This counts how many times each activity appears in each row
activity_summary = train_day[activity_cols].apply(lambda row: row.value_counts(), axis=1).fillna(0)
# Join with the p_num column to group by patient
activity_summary["p_num"] = train_day["p_num"]

activity_columns = [
    "Indoor climbing", "Run", "Strength training", "Swim", "Bike", "Dancing",
    "Stairclimber", "Spinning", "Walking", "HIIT", "Outdoor Bike", "Walk",
    "Aerobic Workout", "Tennis", "Workout", "Hike", "Zumba", "Sport",
    "Yoga", "Swimming", "Weights", "Running"]


#Create unification dictionary

activity_mapping = {
    "Walk": "Walking",
    "Run": "Running",
    "Swim": "Swimming",
}

activity_summary = activity_summary.rename(columns=activity_mapping)
activity_summary = activity_summary.groupby(axis=1, level=0).sum()

print("Create unification dictionary for activities")
print(activity_summary)

activity_by_patient = activity_summary.groupby("p_num").sum(numeric_only=True)
print("Activities are grouped by patient")
activity_by_patient.to_csv("data/activity_by_patient.csv", index= False)
print(activity_by_patient)

#Display how many times (frequency) each type of activity appears per participant.
import matplotlib.pyplot as plt

df_plot = activity_by_patient

df_plot.plot(kind='bar', stacked=True, figsize=(12, 6), colormap='tab20')

plt.title('Activity Frequency by Participant')
plt.xlabel('Participant')
plt.ylabel('Activity Frequency')
plt.legend(loc='upper right', bbox_to_anchor=(1.15, 1)) # adjust the legend if it gets in the way
plt.tight_layout()
#plt.show()

"""
#Compare activity with other variables
"""
#"bg", "insulin", "carbs", "hr", "steps", "cals"
activity_by_patient["bg_mean"] = stats_df["bg_mean"]
activity_by_patient["insulin_mean"] = stats_df["insulin_mean"]
activity_by_patient["carbs_mean"] = stats_df["carbs_mean"]
activity_by_patient["hr_mean"] = stats_df["hr_mean"]
activity_by_patient["steps_mean"] = stats_df["steps_mean"]
activity_by_patient["cals_mean"] = stats_df["cals_mean"]
activity_by_patient["time"] = stats_df["time"]

print(stats_by_patient)
"""
#merge df to analyze activity
"""

"""
#Create a column to sum activities
"""
# List of activity columns to sum
activity_columns = [
    "Indoor climbing", "Running", "Strength training", "Swimming", "Bike", "Dancing",
    "Stairclimber", "Spinning", "HIIT", "Outdoor Bike", "Walking",
    "Aerobic Workout", "Tennis", "Workout", "Hike", "Zumba", "Sport",
    "Yoga", "Weights"]

# Sum the values row-wise and store the result in a new column
stats_by_patient["Activity_sum"] = activity_summary[activity_columns].sum(axis=1)

print(stats_by_patient)
stats_by_patient.to_csv("stats_by_patient_with activities.csv", index=False)


stats_by_patient = pd.read_csv("data/stats_by_patient_with activities.csv")
#plot a heatmap of correlations between all columns ending in '_mean' and the Activity_sum column
cols_interes = [col for col in stats_by_patient.columns if col.endswith('_mean')]
cols_interes.append('Activity_sum')
df_corr = stats_by_patient[cols_interes]
#correlation matrix and heatmap graph

plt.figure(figsize=(10, 8))
sns.heatmap(df_corr.corr(), annot=True, cmap='coolwarm', fmt=".2f", square=True)
plt.title("Correlation heat map")
plt.tight_layout()
#plt.show()

#Hatmap of correlations between columns ending in '_mean' and Activity_sum, grouped by patient cluster
import seaborn as sns
import matplotlib.pyplot as plt
cluster_0 = ['p01', 'p02', 'p04', 'p05', 'p06', 'p10', 'p11']
cluster_1 = ['p03', 'p12']

# Función para graficar por cluster
def plot_cluster_heatmap(pacientes, cluster_id):
    df_cluster = stats_by_patient[stats_by_patient["p_num"].isin(pacientes)]
    df_corr = df_cluster[cols_interes].corr()

    plt.figure(figsize=(10, 8))
    sns.heatmap(df_corr, annot=True, cmap='coolwarm', fmt=".2f", square=True)
    plt.title(f"Mapa de calor de correlaciones - Cluster {cluster_id}")
    plt.tight_layout()
    #plt.show()


# Cluster 0
plot_cluster_heatmap(cluster_0, 0)

# Cluster 1
plot_cluster_heatmap(cluster_1, 1)

"""
#Addition activity variables to compare each one to others.
"""
activity_cols = [col for col in train_day.columns if col.startswith("activity-")]
activity_cols = train_day[activity_cols]

activity_mapping = {
    "Walk": "Walking",
    "Run": "Running",
    "Swim": "Swimming",
}

activity_cols = activity_cols.replace(activity_mapping)
print(activity_cols)
#Concatenamos los df

# Suponiendo que tienes df1 y df2
activity_summary = activity_summary.drop("p_num", axis=1)

df_concatenado = pd.concat([stats_by_patient, activity_summary], axis=1)
print(df_concatenado.columns.tolist())
print(df_concatenado["Tennis"].unique())

#Look every activity and mean of others variables.
activity_columns = [
    'Aerobic Workout', 'Bike', 'Dancing', 'HIIT', 'Hike', 'Indoor climbing',
    'Outdoor Bike', 'Running', 'Spinning', 'Sport', 'Stairclimber',
    'Strength training', 'Swimming', 'Tennis', 'Walking', 'Weights',
    'Workout', 'Yoga', 'Zumba'
]
mean_columns = [col for col in df_concatenado.columns if col.endswith('_mean')]

cols_interes = activity_columns + mean_columns
df_corr = df_concatenado[cols_interes]

plt.figure(figsize=(14, 10))
sns.heatmap(df_corr.corr(), annot=True, cmap='coolwarm', fmt=".2f", square=True)
plt.title("Correlation Heatmap: Activities and _mean Variables")
plt.tight_layout()
plt.show()
"""
"""
#Once we have analyzed the variables without processing the data, we will analyze the time variable.
"""
#Usamos el df al cual hicimos el promedio cada tres columnas
avg_dataset = pd.read_csv("data/df_clean.csv")
df_check= pd.read_csv("data/df_check.csv")
print(avg_dataset.columns.tolist())
#revisamos cada variable en el tiempo
bg = [col for col in avg_dataset.columns if col.startswith("bg_avg_")]
bg = avg_dataset[bg]
#bg["time"] = avg_dataset["time"]
#bg["day_id"] = df_check["day_id"]
bg["period"] = df_check["period"]
#primero un melt
df_melted = bg.melt(id_vars="period",
                             var_name="bg_timepoint",
                             value_name="bg_value")

print(df_melted)

import seaborn as sns
import matplotlib.pyplot as plt

plt.figure(figsize=(12, 6))
sns.scatterplot(data=df_melted, x="period", y="bg_value")
plt.xticks(rotation=45)
plt.title("Evolution of period by time")
plt.tight_layout()
plt.legend().remove()
plt.show()
"""




