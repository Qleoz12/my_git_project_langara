# -*- coding: utf-8 -*-
"""Fuentes en español para celdas 0–28 del notebook A1 (IEEE-CIS EDA)."""
PART1 = []

PART1.append("""#!pip install missingno""")

PART1.append("""import os
import warnings
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import missingno as msno
from scipy import stats
import math


# 1. Bloquear advertencias a nivel de sistema antes de que pandas inicialice por completo
os.environ['PYTHONWARNINGS'] = 'ignore'

# 2. Forzar la supresión de advertencias de formato en Python y pandas
warnings.filterwarnings('ignore')
warnings.simplefilter(action='ignore', category=FutureWarning)

# Semilla global de reproducibilidad según las guías del curso
RANDOM_STATE = 42
np.random.seed(RANDOM_STATE)

# Estándares de visualización
sns.set_theme(style="whitegrid")
plt.rcParams['figure.figsize'] = (12, 6)
plt.rcParams['axes.titlesize'] = 14
plt.rcParams['axes.labelsize'] = 12
""")

PART1.append("""# A1 - Análisis exploratorio de datos (EDA) para selección de variables y reducción de dimensionalidad""")

PART1.append("""## Sección A: Identificación del conjunto de datos y justificación""")

PART1.append("""El conjunto seleccionado para este análisis exploratorio de datos (EDA) es el **IEEE-CIS Fraud Detection Dataset**, proporcionado por Vesta Corporation (referente en soluciones de pago garantizado para comercio electrónico) y alojado por la IEEE Computational Intelligence Society.

En el comercio digital real, la detección de fraude es inherentemente compleja: desbalance extremo de clases, volumen masivo, alta dimensionalidad y gran cantidad de valores faltantes por privacidad o limitaciones del sistema. Este conjunto benchmark contiene transacciones de comercio electrónico a gran escala, con variables que van desde características del dispositivo hasta metadatos de la transacción, lo que lo hace ideal para estrategias rigurosas de selección de variables y reducción de dimensionalidad.""")

PART1.append("""**Cita**

@misc{ieee-fraud-detection,
    author = {Addison Howard and Bernadette Bouchon-Meunier and IEEE CIS and inversion and John Lei and Lynn@Vesta and Marcus2010 and Prof. Hussein Abbass},
    title = {IEEE-CIS Fraud Detection},
    year = {2019},
    howpublished = {\\url{https://kaggle.com/competitions/ieee-fraud-detection}},
    note = {Kaggle}
}""")

PART1.append("""## Sección B: Diccionario de datos""")

PART1.append("""El conjunto IEEE-CIS Fraud Detection sigue un esquema relacional con dos archivos principales enlazados por un identificador único de transacción: `train_transaction.csv` y `train_identity.csv`. Las variables se organizan, clasifican y definen operativamente a continuación según su tipo de dato y su escala estadística.""")

PART1.append("""# 1. Carga de archivos en DataFrames de Pandas
# Ajusta las rutas si tus archivos están en una subcarpeta, p. ej. 'data/'
train_transaction = pd.read_csv('train_transaction.csv')
train_identity = pd.read_csv('train_identity.csv')

print(f"Matriz de transacciones cargada: {train_transaction.shape[0]} filas, {train_transaction.shape[1]} variables.")
print(f"Matriz de identidad cargada: {train_identity.shape[0]} filas, {train_identity.shape[1]} variables.")

# 2. Combinación relacional mediante left join sobre la llave principal
# Garantiza no perder transacciones sin registros correspondientes en identidad
df_train = pd.merge(train_transaction, train_identity, on='TransactionID', how='left')

print("-" * 60)
print(f"Forma de la matriz EDA consolidada: {df_train.shape[0]} filas y {df_train.shape[1]} variables.")
""")

PART1.append("""# 1. Lista de columnas representativas que cubren los bloques del esquema
representative_features = [
    'TransactionID', 'TransactionDT', 'TransactionAmt',
    'ProductCD', 'card1', 'card4', 'addr1', 'dist1',
    'P_emaildomain', 'M1', 'C1', 'D1', 'V1', 'V101',
    'DeviceType', 'DeviceInfo', 'id_01', 'id_13', 'id_19', 'id_38'
]

# 2. Filtrar a columnas presentes en el entorno de trabajo
active_features = [col for col in representative_features if col in df_train.columns]

# 3. Extraer metadatos estructurales desde el DataFrame
metadata_records = []
for col in active_features:
    storage_type = str(df_train[col].dtype)
    missing_count = df_train[col].isna().sum()

    # Tipo lógico según propiedades analíticas
    if storage_type in ['object', 'bool'] or col in ['TransactionID', 'isFraud', 'addr1', 'addr2']:
        logical_type = 'Categórica'
    else:
        logical_type = 'Numérica'

    metadata_records.append({
        'Nombre de variable': col,
        'Tipo lógico': logical_type,
        'Tipo almacenamiento (Python)': storage_type,
        'Nulos activos (conteo)': missing_count
    })

# 4. DataFrame de verificación tipológica
df_type_justification = pd.DataFrame(metadata_records)

# 5. Mostrar tabla
print("=== MATRIZ DE TIPOS DE ALMACENAMIENTO ===")
display(df_type_justification)""")

PART1.append("""---

### B1 - Identificadores operativos clave y variable objetivo

| Nombre de variable | Tipo de dato (lógico) | Tipo de almacenamiento (Python) | Escala de medición | Descripción operativa y lógica de dominio |
| :--- | :--- | :--- | :--- | :--- |
| **TransactionID** | Categórica | Entero | Nominal (identificador) | Llave única por transacción. Sirve para unir las tablas de transacción e identidad. |
| **isFraud** | Categórica | Binaria / booleana | Nominal (objetivo) | Variable objetivo. Indica si la transacción fue fraudulenta (**1**) o legítima (**0**). |

---

### B2 - Variables de la tabla de transacciones (`train_transaction.csv`)

| Grupo de variables | Tipo de dato (lógico) | Tipo de almacenamiento (Python) | Escala de medición | Descripción operativa y lógica de dominio |
| :--- | :--- | :--- | :--- | :--- |
| **TransactionDT** | Numérica | Flotante / entero | Continua | Proxy temporal: segundos transcurridos desde una fecha-referencia no revelada (no es un sello temporal absoluto). |
| **TransactionAmt** | Numérica | Flotante | Continua | Monto del pago en USD (o equivalente en moneda local). |
| **ProductCD** | Categórica | Cadena / object | Nominal | Código o categoría de producto por transacción (p. ej. W, H, C, R). |
| **card1 - card6** | Categórica | Entero / cadena | Nominal | Información de tarjeta (tipo, red emisora, banco emisor aprox., país). |
| **addr1, addr2** | Categórica | Flotante / entero | Nominal | Códigos geográficos; `addr1` suele corresponder a región postal de facturación y `addr2` a país. |
| **P_emaildomain**<br>**R_emaildomain** | Categórica | Cadena / object | Nominal | Dominios de correo del comprador (**P**) y del receptor (**R**). |
| **M1 - M9** | Categórica | Cadena / object | Nominal | Indicadores de coincidencias (p. ej. T/F u otros códigos) entre datos de facturación, nombres y tarjetas. |
| **C1 - C14** | Numérica | Entero / flotante | Discreta | Conteos de comportamiento (cuántas veces se observa tarjeta, correo, dispositivo, etc.). |
| **D1 - D15** | Numérica | Flotante | Continua | Diferencias temporales (días o intervalos entre la transacción actual y registros previos). |
| **dist1, dist2** | Numérica | Flotante | Continua | Métricas de distancia física entre atributos (facturación, envío, códigos postales, IP, teléfono, según ingeniería de la tabla). |

---

### B3 - Variables comportamentales V de Vesta (bloque V)

| Grupo de variables | Tipo de dato (lógico) | Tipo de almacenamiento (Python) | Escala de medición | Descripción operativa y lógica de dominio |
| :--- | :--- | :--- | :--- | :--- |
| **V1 - V339** | Numérica | Flotante | Continua / discreta | Variables anonimizadas e ingenierizadas por Vesta: clasificaciones, conteos y puntuaciones de relación entre atributos de pago y comportamiento histórico. |

---

### B4 - Variables de la tabla de identidad (`train_identity.csv`)

| Grupo de variables | Tipo de dato (lógico) | Tipo de almacenamiento (Python) | Escala de medición | Descripción operativa y lógica de dominio |
| :--- | :--- | :--- | :--- | :--- |
| **DeviceType** | Categórica | Cadena / object | Nominal | Entorno del cliente (móvil, escritorio). |
| **DeviceInfo** | Categórica | Cadena / object | Nominal | Cadena de hardware/software (p. ej. Windows, iOS, modelo de dispositivo). |
| **id_01 - id_11** | Numérica | Flotante | Continua / discreta | Marcadores numéricos (red, resolución, señales de comportamiento técnico). |
| **id_12 - id_38** | Categórica | Cadena / object | Nominal | Indicadores categóricos de identidad digital (navegador, proxy, SO, disponibilidad de credenciales, etc.). |""")

PART1.append("""## Sección C: Evaluación de la calidad de datos""")

PART1.append("""### C1 - Evaluación de registros duplicados""")

PART1.append("""Antes del perfil descriptivo o pruebas de ausencia sistemática conviene revisar la integridad básica del conjunto. Este paso cuantifica duplicidad de filas en el conjunto fusionado usando la llave principal (**TransactionID**). Detectar y eliminar duplicados evita distorsionar la varianza y los análisis univariados posteriores.""")

PART1.append("""# Sección C: Evaluación de la calidad de datos
# Subsección: Registros duplicados

# 1. Contar duplicados exactos según TransactionID
duplicate_count = df_train.duplicated(subset=['TransactionID']).sum()
duplicate_percentage = (duplicate_count / len(df_train)) * 100

print("--- Evaluación de registros duplicados ---")
print(f"Duplicados detectados en total: {duplicate_count}")
print(f"Porcentaje de duplicados estructurales: {duplicate_percentage:.4f}%")

# 2. Limpieza programática si aplica
if duplicate_count > 0:
    df_train = df_train.drop_duplicates(subset=['TransactionID'])
    print("Se eliminaron filas duplicadas para preservar una fila única por transacción.")
else:
    print("No se requirió limpieza por duplicados. Cada fila representa un evento único.")
""")

PART1.append("""### C2 - Codificación inconsistente y canonicalización de cadenas""")

PART1.append("""Las entradas de texto inconsistentes (espacios finales o mayúsculas/minúsculas mezcladas) pueden inflar artificialmente la cardinalidad de campos categóricos. Se revisan variables clave (`ProductCD`, `card4`, `DeviceType`) para confirmar etiquetas limpias y estables.""")

PART1.append("""# Subsección: Evaluación de codificación inconsistente

# Variables categóricas para inspección
categorical_integrity_checks = ['ProductCD', 'card4', 'DeviceType']

print("--- Perfil de consistencia categórica ---")
for feature in categorical_integrity_checks:
    if feature in df_train.columns:
        unique_labels = df_train[feature].unique()
        raw_cardinality = df_train[feature].nunique()

        print(f"\\nPerfil de '{feature}' | cardinalidad: {raw_cardinality}")
        print(f"Etiquetas únicas: {unique_labels}")

        if df_train[feature].dtype == 'object':
            normalized_count = df_train[feature].astype(str).str.strip().str.lower().nunique()
            if normalized_count != raw_cardinality:
                print(f"⚠️ Posible inconsistencia espacial/formato en '{feature}'.")
""")

PART1.append("""### C3 - Valores fuera de rango y diagnóstico de valores atípicos""")

PART1.append("""Se aísla el indicador financiero (**TransactionAmt**) para validar el dominio (p. ej. montos no negativos y sentido económico) y fijar un umbral de valores extremos mediante el método del rango intercuartílico (RIC / IQR).""")

PART1.append("""# Sección C: Evaluación de la calidad de datos
# Subsección: Diagnóstico visual de atípicos con diagramas de caja

fig, axes = plt.subplots(1, 2, figsize=(16, 6))

# --- Gráfico A: Monto de transacción (escala cruda) ---
sns.boxplot(data=df_train, x='TransactionAmt', ax=axes[0], color='teal')
axes[0].set_title("Distribución y valores atípicos del monto de transacción (escala original)")
axes[0].set_xlabel("Monto de transacción (USD)")

# --- Gráfico B: Escala logarítmica ---
sns.boxplot(data=df_train, x='TransactionAmt', ax=axes[1], color='lightblue')
axes[1].set_xscale('log')
axes[1].set_title("Perfil de atípicos del monto (escala log)")
axes[1].set_xlabel("Log del monto de transacción (USD)")

plt.tight_layout()
plt.show()

# Límites matemáticos IQR
Q1 = df_train['TransactionAmt'].quantile(0.25)
Q3 = df_train['TransactionAmt'].quantile(0.75)
IQR = Q3 - Q1
upper_bound = Q3 + 1.5 * IQR

extreme_outliers_count = df_train[df_train['TransactionAmt'] > upper_bound].shape[0]

print("--- Límites matemáticos por rango intercuartílico (IQR) ---")
print(f" - Cuartil inferior (Q1): {Q1:.2f} USD")
print(f" - Cuartil superior (Q3): {Q3:.2f} USD")
print(f" - IQR: {IQR:.2f} USD")
print(f" - Límite superior (Q3 + 1.5×IQR): {upper_bound:.2f} USD")
print(f" - Observaciones por encima del límite: {extreme_outliers_count}")
""")

PART1.append("""\nBasándome en los diagnósticos visuales y cuantitativos, las 66 482 observaciones identificadas como valores atípicos extremos **no** se eliminan del conjunto por las siguientes razones:\n\n* **Preservación de señal de fraude:** En pagos electrónicos, valores altos pueden asociarse a fraude; los agentes pueden intentar movimientos de gran monto antes de que la tarjeta sea bloqueada. Eliminar esos casos borraría señales que los modelos deben aprender.\n* **Datos realistas:** No se trata de errores de captura necesariamente sino de eventos creíbles de alto valor.\n* **Mitigación aguas abajo:** En lugar de depuración por borrado, la asimetría extrema se abordará con transformaciones robustas (`log`, `RobustScaler`) y modelos poco sensibles a atípicos (bosques aleatorios, LightGBM, etc.).""")

PART1.append("""### C4 - Contraste de mecanismos de ausencia de datos (MCAR frente a MAR/MNAR)""")

PART1.append("""#### DeviceType""")

PART1.append("""Evalúo qué patrón estadístico rige los datos ausentes: no todas las transacciones generan bitácoras de identidad. Se plantea la hipótesis de que los faltantes (p. ej. en **DeviceType**) dependen estructuralmente de la clase **isFraud** (MAR o MNAR) y no de un proceso puramente aleatorio (MCAR). Para la rúbrica se ejecuta chi-cuadrado de independencia reportando estadístico y valor p.""")

PART1.append("""# Subsección: Prueba de patrones de ausencia
# H0: la ausencia en identidad es independiente de la clase objetivo (MCAR).
# H1: la ausencia depende de la legitimidad de la transacción (MAR o MNAR).

print("--- Inferencia estadística sobre ausencia de datos ---")

df_train['DeviceType_Missing'] = df_train['DeviceType'].isna()

contingency_matrix = pd.crosstab(df_train['DeviceType_Missing'], df_train['isFraud'])
print("\\nTabla de contingencia (DeviceType ausente vs. isFraud):")
print(contingency_matrix)

chi2_stat, p_value, dof, expected_freq = stats.chi2_contingency(contingency_matrix)

print("\\n--- Resultados chi-cuadrado ---")
print(f"Estadístico chi-cuadrado: {chi2_stat:.4f}")
print(f"Valor p asintótico: {p_value:.4e}")
""")

PART1.append("""**Conclusión**: Se rechaza H0 al 95 % de confianza. Los faltantes de `DeviceType` dependen de `isFraud`, lo cual refuta MCAR y apunta a un mecanismo **MAR/MNAR** (p. ej. captura diferencial de especificaciones o rutas donde no se exige huella de identidad).""")

PART1.append("""#### Proximidad espacial (dist1)""")

PART1.append("""Para contrastar patrones entre bloques independientes replique el mismo enfoque con **dist1** (métrica de distancia física declarada por la tabla). Suele aparecer ausencia por límites de enruteo o fronteras transfronterizas. La hipótesis formal es que los faltantes en `dist1` se asocian estadísticamente con `isFraud`, estableciendo contexto MAR distinto al canal de identidad.""")

PART1.append("""# 1. Indicador ausente-binario para dist1
df_train['dist1_Missing'] = df_train['dist1'].isna()

# 2. Tabla vs. clase objetivo
dist_contingency = pd.crosstab(df_train['dist1_Missing'], df_train['isFraud'])
print("\\nTabla de contingencia (dist1 ausente vs. isFraud):")
print(dist_contingency)

# 3. Chi-cuadrado
chi2_stat_dist, p_value_dist, dof_dist, expected_dist = stats.chi2_contingency(dist_contingency)

print("\\n--- Resultados chi-cuadrado (dist1) ---")
print(f"Estadístico chi-cuadrado: {chi2_stat_dist:.4f}")
print(f"Valor p asintótico: {p_value_dist:.4e}")
""")
