# -*- coding: utf-8 -*-
"""Fuentes en español para celdas 30–56 del notebook A1."""

PART2 = []

# --- Celda 30 ---
PART2.append(
    """**Conclusión**: Se rechaza la hipótesis nula (H0) al 95 % de confianza. El patrón de ausencia en `dist1` depende de `isFraud`; el proceso no cumple MCAR. Es compatible con **MAR**, lo que sugiere que algunos flujos de pago/productos pueden no calcular distancia física y, a la vez, presentar probabilidades diferenciales de fraude."""
)

# --- Celda 31 ---
PART2.append(
    """\
# 1. Indicadores de ausencia
df_train['DeviceType_Missing'] = df_train['DeviceType'].isna()
df_train['dist1_Missing'] = df_train['dist1'].isna()

# 2. Tasas de ausencia por clase de fraude
dev_missing_rates = df_train.groupby('isFraud')['DeviceType_Missing'].mean().reset_index()
dist_missing_rates = df_train.groupby('isFraud')['dist1_Missing'].mean().reset_index()

# 3. Gráficas de barras comparativas
fig, axes = plt.subplots(1, 2, figsize=(16, 6))

# Gráfico A: DeviceType
sns.barplot(
    data=dev_missing_rates,
    x='isFraud',
    y='DeviceType_Missing',
    ax=axes[0],
    palette='coolwarm',
    hue='isFraud',
    legend=False,
)
axes[0].set_title(
    "Probabilidad empírica de ausencia — matrices de identidad (DeviceType)"
)
axes[0].set_xlabel("Clase (0: legítimo, 1: fraude)")
axes[0].set_ylabel("Proporción de filas con valor nulo")
axes[0].set_ylim(0, 1.0)

for p in axes[0].patches:
    axes[0].annotate(
        f'{100 * p.get_height():.2f}%',
        (p.get_x() + p.get_width() / 2.0, p.get_height() + 0.02),
        ha='center',
        va='center',
        fontsize=11,
        color='black',
        fontweight='bold',
    )

# Gráfico B: dist1
sns.barplot(
    data=dist_missing_rates,
    x='isFraud',
    y='dist1_Missing',
    ax=axes[1],
    palette='coolwarm',
    hue='isFraud',
    legend=False,
)
axes[1].set_title("Probabilidad empírica de ausencia — distancia espacial (dist1)")
axes[1].set_xlabel("Clase (0: legítimo, 1: fraude)")
axes[1].set_ylabel("Proporción de filas con valor nulo")
axes[1].set_ylim(0, 1.0)

for p in axes[1].patches:
    axes[1].annotate(
        f'{100 * p.get_height():.2f}%',
        (p.get_x() + p.get_width() / 2.0, p.get_height() + 0.02),
        ha='center',
        va='center',
        fontsize=11,
        color='black',
        fontweight='bold',
    )

plt.tight_layout()
plt.show()
"""
)

# --- Celda 32 ---
PART2.append(
    """La visualización bifocal refuta el supuesto MCAR (**Missing Completely at Random**) y muestra ausencias muy informativas:

* **`DeviceType` (identidad digital):** en operaciones presumiblemente legítimas la tasa de nulos llega en torno al **77,22 %**, mientras entre fraudulentas cae a **45,74 %**. El patrón sugiere **MAR/MNAR**, asociado a rutas donde el motor de seguridad sólo registra huella cuando el score de riesgo es alto.

* **`dist1` (proximidad espacial):** la ausencia sube desde **47,28 %** (no fraude) a **65,17 %** en fraude, compatible con rutas donde redirecciones tipo VPN o infraestructura transfronteriza impiden obtener distancias.

Al ser predictivas estas ausencias conviene preservar banderas de faltantes o tratamientos que respeten la dispersión dentro de clasificadores en árbol."""
)

# --- Celda 33 ---
PART2.append("""## Sección D: Análisis exploratorio de datos""")

# --- Celda 34 ---
PART2.append(
    """### D1 — Análisis univariado: variables numéricas"""
)

# --- Celda 35 ---
PART2.append("""#### TransactionAmt y TransactionDT""")

# --- Celda 36 ---
PART2.append(
    """Analizo el comportamiento marginal de **`TransactionAmt`** (monto) y **`TransactionDT`** (eje temporal relativo): histogramas con estimación KDE y diagramas de caja para sintetizar asimetría y colas pesadas."""
)

# --- Celda 37 ---
PART2.append(
    """\
# Sección D: exploración — D1 variables numéricas

fig, axes = plt.subplots(2, 2, figsize=(16, 12))

# TransactionAmt — histogramas
sns.histplot(
    data=df_train, x='TransactionAmt', bins=100, kde=True, ax=axes[0, 0], color='teal'
)
axes[0, 0].set_title("Distribución del monto (escala original)")
axes[0, 0].set_xlabel("Monto de transacción (USD)")
axes[0, 0].set_ylabel("Frecuencia")

sns.histplot(
    data=df_train, x='TransactionAmt', bins=100, kde=True, ax=axes[0, 1], color='teal'
)
axes[0, 1].set_yscale('log')
axes[0, 1].set_title("Distribución del monto — escala log en frecuencia")
axes[0, 1].set_xlabel("Monto de transacción (USD)")
axes[0, 1].set_ylabel("Log(frecuencia)")

# TransactionDT
sns.histplot(
    data=df_train, x='TransactionDT', bins=50, kde=True, ax=axes[1, 0], color='darkblue'
)
axes[1, 0].set_title("Distribución del delta temporal TransactionDT")
axes[1, 0].set_xlabel("Tiempo transcurrido (segundos desde epoch de referencia)")
axes[1, 0].set_ylabel("Frecuencia")

sns.boxplot(data=df_train, x='TransactionDT', ax=axes[1, 1], color='lightblue')
axes[1, 1].set_title("Diagrama de cuartiles — TransactionDT")
axes[1, 1].set_xlabel("Tiempo transcurrido (segundos)")

plt.tight_layout()
plt.show()

print("--- Estadísticas descriptivas ---")
print(df_train[['TransactionAmt', 'TransactionDT']].describe())
"""
)

# --- Celda 38 ---
PART2.append("""#### dist1, C1 y C2""")

# --- Celda 39 ---
PART2.append(
    """Construyo histogramas con KDE y diagramas de caja para `dist1` (proximidad), `C1` y `C2` (conteos de comportamiento). La fuerte asimetría fuerza vistas lineales antes de aplicar ejes transformados donde el lienzo saturado lo requiera."""
)

# --- Celda 40 ---
PART2.append(
    """\
# Sección D — D1 (vistas en escala lineal cruda)


fig, axes = plt.subplots(3, 2, figsize=(16, 15))

# dist1
sns.histplot(data=df_train, x='dist1', bins=50, kde=True, ax=axes[0, 0], color='teal')
axes[0, 0].set_title("Distribución de dist1 (escala original)")
axes[0, 0].set_xlabel("Métrica de distancia")

sns.boxplot(data=df_train, x='dist1', ax=axes[0, 1], color='lightblue')
axes[0, 1].set_title("Diagrama de caja — dist1 (linear)")
axes[0, 1].set_xlabel("Distancia")

# C1
sns.histplot(data=df_train, x='C1', bins=50, kde=True, ax=axes[1, 0], color='darkblue')
axes[1, 0].set_title("Distribución del conteo C1 (escala original)")
axes[1, 0].set_xlabel("Frecuencias / conteos")

sns.boxplot(data=df_train, x='C1', ax=axes[1, 1], color='royalblue')
axes[1, 1].set_title("Diagrama de caja — C1 (linear)")
axes[1, 1].set_xlabel("Valor de C1")

# C2
sns.histplot(data=df_train, x='C2', bins=50, kde=True, ax=axes[2, 0], color='purple')
axes[2, 0].set_title("Distribución del conteo C2 (escala original)")
axes[2, 0].set_xlabel("Frecuencias / conteos")

sns.boxplot(data=df_train, x='C2', ax=axes[2, 1], color='plum')
axes[2, 1].set_title("Diagrama de caja — C2 (linear)")
axes[2, 1].set_xlabel("Valor de C2")

plt.tight_layout()
plt.show()
"""
)

# --- Celda 41 ---
PART2.append(
    """\
# Sección D — D1 con transformaciones para interpretación

fig, axes = plt.subplots(3, 2, figsize=(16, 15))

# dist1
sns.histplot(data=df_train, x='dist1', bins=50, kde=True, ax=axes[0, 0], color='teal')
axes[0, 0].set_yscale('log')
axes[0, 0].set_title("Distribución de dist1 — escala log en frecuencia")
axes[0, 0].set_xlabel("Métrica de distancia")

sns.boxplot(data=df_train, x='dist1', ax=axes[0, 1], color='lightblue')
axes[0, 1].set_xscale('log')
axes[0, 1].set_title("Cuartiles — dist1 (log en variable)")
axes[0, 1].set_xlabel("Distancia (log)")

# C1
sns.histplot(data=df_train, x='C1', bins=50, kde=True, ax=axes[1, 0], color='darkblue')
axes[1, 0].set_yscale('log')
axes[1, 0].set_title("Distribución de C1 — log en conteo de barras")
axes[1, 0].set_xlabel("Valores numéricos de C1")

sns.boxplot(data=df_train, x='C1', ax=axes[1, 1], color='royalblue')
axes[1, 1].set_xscale('log')
axes[1, 1].set_title("Diagrama de caja — C1")
axes[1, 1].set_xlabel("Valor de C1 (escala log)")

# C2
sns.histplot(data=df_train, x='C2', bins=50, kde=True, ax=axes[2, 0], color='purple')
axes[2, 0].set_yscale('log')
axes[2, 0].set_title("Distribución de C2 — escala log en frecuencia")
axes[2, 0].set_xlabel("Valores numéricos de C2")

sns.boxplot(data=df_train, x='C2', ax=axes[2, 1], color='plum')
axes[2, 1].set_xscale('log')
axes[2, 1].set_title("Diagrama de caja — C2")
axes[2, 1].set_xlabel("Valor de C2 (escala log)")

plt.tight_layout()
plt.show()

display(df_train[['dist1', 'C1', 'C2']].describe())
"""
)

# --- Celda 42 ---
PART2.append(
    """\
Durante el seguimiento univariado (`dist1`, `C1`, `C2`) la vista lineal provocó el típico *efecto rascacielos*: más del 95 % de las observaciones colapsan en los primeros contenedores ante colas muy pesadas. Para cumplir estándares de EDA aplico **ejes transformados logarítmicos** donde aportó claridad (frecuencia o valores según caso)."""
)

# --- Celda 43 ---
PART2.append(
    """### D2 — Análisis univariado: variables categóricas"""
)

# --- Celda 44 ---
PART2.append(
    """Selecciono un conjunto nominal representativo cubriendo transacciones (`ProductCD`), tarjetas (`card4`, `card6`), verificación (`M4`) y marca dispositivo/identidad (`DeviceType`, `id_12`, `id_15`, `id_38`)."""
)

PART2.append(
    """\
from IPython.display import display, HTML

disable_scroll_css = '''
<style>
    .output_scroll, .output_wrapper, .jp-OutputArea-child, .jp-Cell-outputArea {
        height: auto !important;
        max-height: none !important;
        overflow: visible !important;
    }
</style>
'''
display(HTML(disable_scroll_css))

categorical_pool = [
    'ProductCD', 'card4', 'card6', 'M4',
    'DeviceType', 'id_12', 'id_15', 'id_38'
]

active_categorical = [col for col in categorical_pool if col in df_train.columns]
num_plots = len(active_categorical)
num_cols = 2
num_rows = math.ceil(num_plots / num_cols)

fig, axes = plt.subplots(num_rows, num_cols, figsize=(16, 6 * num_rows))
axes = axes.flatten()

total_observations = len(df_train)

for i, col in enumerate(active_categorical):
    ax = axes[i]

    sorted_order = df_train[col].dropna().value_counts().index

    sns.countplot(
        data=df_train,
        x=col,
        hue=col,
        order=sorted_order,
        ax=ax,
        palette='viridis' if i % 2 == 0 else 'magma',
        legend=False,
    )

    ax.set_title(
        f"Distribución empírica: {col}", fontsize=13, fontweight='bold'
    )
    ax.set_xlabel(f"Categorías de {col}", fontsize=11)
    ax.set_ylabel("Observaciones", fontsize=11)

    try:
        max_bar_height = df_train[col].value_counts().max()
        y_offset = max_bar_height * 0.03
    except Exception:
        y_offset = 1000

    for p in ax.patches:
        h = p.get_height()
        if h > 0:
            percentage_string = f'{100 * h / total_observations:.2f}%'
            ax.annotate(
                percentage_string,
                (p.get_x() + p.get_width() / 2.0, h + y_offset),
                ha='center',
                va='bottom',
                fontsize=10,
                color='black',
                fontweight='bold',
            )

    if len(sorted_order) > 3:
        ax.tick_params(axis='x', rotation=20)

for j in range(num_plots, len(axes)):
    fig.delaxes(axes[j])

plt.tight_layout()
plt.show()

print("=== MÉTRICAS DE DISTRIBUCIONES CATEGÓRICAS ===")
for col in active_categorical:
    print(f"\\nFrecuencias absolutas '{col}':")
    print(df_train[col].value_counts(dropna=False))
"""
)

PART2.append("""### D3 — Relaciones bivariadas numérica–numérica""")

PART2.append(
    """Analizo la co-distribución marginal de métricas clave usando **Spearman ρ** frente a Pearson: así captamos relaciones monótonas bajo asimetría severa/outliers (`TransactionAmt`, `TransactionDT`, `C1`/`C2` y un ejemplo `V101`–`V103`). Complemento la matriz con dispersión objetivo montos vs tiempo en escala log para el eje monetario."""
)

PART2.append(
    """\
# Sección D — D3 correlaciones numericas

numerical_subset = [
    'TransactionAmt', 'TransactionDT',
    'C1', 'C2',
    'V101', 'V102', 'V103'
]
valid_numerical_features = [col for col in numerical_subset if col in df_train.columns]

correlation_matrix = df_train[valid_numerical_features].corr(method='spearman')

fig, axes = plt.subplots(1, 2, figsize=(18, 7))

sns.heatmap(
    data=correlation_matrix,
    annot=True,
    fmt=".3f",
    cmap="coolwarm",
    vmin=-1,
    vmax=1,
    square=True,
    ax=axes[0],
    cbar_kws={"label": "Coeficiente de correlacion rho (Spearman)"},
)
axes[0].set_title("Heatmap rho de Spearman (subconjunto numerico)")

sns.scatterplot(
    data=df_train,
    x='TransactionDT',
    y='TransactionAmt',
    alpha=0.1,
    ax=axes[1],
    color='darkblue',
)
axes[1].set_yscale('log')
axes[1].set_title("Dispersion: monto vs delta temporal TransactionDT")
axes[1].set_xlabel("Segundos transcurridos (epoch referencia)")
axes[1].set_ylabel("Logaritmo decimal del monto (USD)")

plt.tight_layout()
plt.show()

print("--- Matriz rho de Spearman ---")
print(correlation_matrix)
"""
)

PART2.append(
    """**Conclusiones**
========================>

Las correlaciones de Spearman indican alta asociacion monotona entre `V101`/`V102`/`V103` y fuerte relacion positiva tambien entre `C1` y `C2`. En cambio, `TransactionAmt` y `TransactionDT` apenas acumulan señales monotonicas coherentes contra el cuadro mostrado, lo cual concuerda con el diagrama disperso donde los montos muestran ciclos de densidad pero no tendencia marcada contra el tiempo relativo."""
)

PART2.append("""### D4 — Relaciones bivariadas variable ↔ objetivo `isFraud`""")

PART2.append(
    """Se estratifican variables continuas frente al objetivo binario y se grafican cruces categoricos cuando aplica (`ProductCD`). Cada grafico viene acompanado formalmente por pruebas Mann-Whitney U y chi cuadrado declarando estadistico y valor p."""
)

PART2.append(
    """\
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from scipy import stats

fig, axes = plt.subplots(1, 2, figsize=(18, 7))

sns.boxplot(
    data=df_train,
    x='isFraud',
    y='TransactionAmt',
    ax=axes[0],
    palette='Set2',
    hue='isFraud',
    legend=False,
)
axes[0].set_yscale('log')
axes[0].set_title("Distribucion del monto segun clase de fraude")
axes[0].set_xlabel("Clase (0 legítimo, 1 fraude)")
axes[0].set_ylabel("Logaritmo decimal del monto (USD)")

product_fraud_rates = (
    df_train.groupby('ProductCD')['isFraud'].mean().reset_index().sort_values(by='isFraud', ascending=False)
)

sns.barplot(
    data=product_fraud_rates,
    x='ProductCD',
    y='isFraud',
    ax=axes[1],
    palette='viridis',
    hue='ProductCD',
    legend=False,
)
axes[1].set_title("Tasa observada de fraude por ProductCD")
axes[1].set_xlabel("Codigo ProductCD")
axes[1].set_ylabel("Fracción fraudulenta")

for p in axes[1].patches:
    pct = f'{100 * p.get_height():.2f}%'
    axes[1].annotate(
        pct,
        (
            p.get_x() + p.get_width() / 2.0,
            p.get_height() + 0.002,
        ),
        ha='center',
        va='center',
        fontsize=11,
        color='black',
        fontweight='bold',
    )

plt.tight_layout()
plt.show()

print("=" * 70)
print("--- PRUEBAS DE HIPOTESIS ---")
print("=" * 70)

amt_ok = df_train[df_train['isFraud'] == 0]['TransactionAmt']
amt_fraud = df_train[df_train['isFraud'] == 1]['TransactionAmt']
mwu_stat, mwu_p = stats.mannwhitneyu(amt_ok, amt_fraud, alternative='two-sided')
print("\\n[Test 1] Mann-Whitney U (Montos vs isFraud):")
print(f" - Estadistico U: {mwu_stat:.4f}")
print(f" - p-valor asintótico: {mwu_p:.4e}")
if mwu_p < 0.05:
    print(" - Decision: rechazar H0 — los montos difieren entre categorias observadas.")
else:
    print(" - Decision: no rechazar H0 — sin evidencia de cambio marcado.")

product_contingency = pd.crosstab(df_train['ProductCD'], df_train['isFraud'])
chi2_stat, chi2_p, dof, expected = stats.chi2_contingency(product_contingency)

print("\\n[Test 2] Chi cuadrado ProductCD × isFraud:")
print(f" - Chi cuadrado observado: {chi2_stat:.4f}")
print(f" - p-valor: {chi2_p:.4e}")
print(f" - Grados de libertad: {dof}")
if chi2_p < 0.05:
    print(" - Decision: rechazar H0 — las tasas de fraude cambian sistematicamente con producto.")
else:
    print(" - Decision: no rechazar H0 — tasas estadisticamente estables contra producto nominal.")
"""
)

PART2.append(
    """**Conclusiones**

==============>

Los paneles sugieren efectos diferentes por canal financiero cuando el problema se expresa mediante tasas y la significancia global proviene de chi cuadrado con multitud de grados libertad por categorias dispersas — conviene cualificar tamaño efecto usando métricas de negocio o modelos siguientes antes de extrapolar causa raiz especifica."""
)

PART2.append("""## Sección E: Formulación del problema""")

PART2.append(
    r"""


Basándome en el EDA y, para el marco conceptual, prácticas de estratificación con etiquetas retrospectivas similares a las discutidas en literatura prognóstica (p. ej. Laqueur et al., 2022), definimos formalmente el problema de datos.


---

### 1. Contexto operativo y de negocio
En pagos electrónicos el fraude implica pérdidas de ingreso, cargas antifraude y deterioro de la confianza de clientes e instituciones (incluyendo procesadores como Vesta en el mismo ecosistema de la competencia). Por el contrario, reglas sobre-agresivas elevan falsos rechazos y fricción al declinar cargos válidos.


El objetivo operativo principal es usar, a gran escala, registros administrativos de transacción e identificación digital anonimizada para aislar sistemáticamente señales de fraude respecto comportamientos de compra benignos típicamente legítimos.


---

### 2. Formalización estadística / machine learning

Se formula como una **clasificación supervisada binaria** bajo **desbalance marcado de clases** (aprox. 3.5 % de positivos).


* **Instancia observada \(i\)**: consolidación mediante la llave única después de hacer merge transacciones con identidades.

* **Espacio predictor \(X\)**: vector \( X_i\in\mathbb{R}^{d}\) con \( d\approx433 \) combinando valores financieros (`TransactionAmt`), tiempos relativos (`TransactionDT`, grupos `D`), conteos comportamentales (`C`), chequeos texto (`M`) y marcadores tecnológicos (`DeviceType`, `DeviceInfo`).



* **Variable objetivo \(Y_i\)**: etiqueta binaria igual que define el archivo de competencia:

  $$Y_i = \begin{cases} 
  1 & \text{si la historia del dataset marca fraude;} \\ 
  0 & \text{si la marca es transacción legítima.}
  \end{cases}$$

Propósito: estimar probabilidades \(\hat{p}_i = \mathbb{P}(Y_i=1 \mid X_i)\) mediante modelos adecuados a alta dimensionalidad (por ejemplo ensembles en árbol o modelos lineales regularizados, eventualmente después de PCA u otra reducción controlada).


---

### 3. Ventana temporal (lectura habitual del problema)

* **Momento del score:** \(\hat{p}_i\) debe interpretarse con la información observable en el mismo instante relativo sintetizado por `TransactionDT`.
* **Formación tardía del objetivo \(Y\):** positivos habitualmente llegan mediante confirmaciones post-transacción (p. ej. chargebacks registrados después), preservando orden causal entre rasgos conocidos primeramente y adjudicaciones posteriores.


---

### 4. Métricas de rendimiento conscientes del desbalance

Accuracy simple es engañosa cuando la clase mayoritaria domina conteos muestrales (modelos triviales predicen sólo clase negativa y logran alta exactitud).


1. **AUC ROC (AUROC)** para ordenar probabilidades discriminando globalmente.
2. **AUC PR (AUPRC)** como métrica complementaria enfocada a la clase minoritaria ante prevalencia baja.
3. **F-score y herramientas tipo índice de Youden** para fijar umbrales operativos condicionados a políticas corporativas (maximizar detección frente a contener rechazos equivocados).


"""
)

PART2.append("")
