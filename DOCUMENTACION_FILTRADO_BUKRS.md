# Documentación del Filtrado de BUKRS (Sociedades)

## Ubicación del Código

El código de filtrado de BUKRS se encuentra en el archivo: **`zfi_extraction_citems.abap`**

### Secciones Principales:

1. **Líneas 20-29**: Definición del tipo `ty_bukrs_range` para rangos de sociedades
2. **Líneas 40-42**: Declaración de tablas internas para el filtrado
3. **Líneas 130-192**: Formulario `PREPARE_BUKRS_FILTER` (código principal de filtrado)
4. **Línea 227**: Aplicación del filtro en la consulta SQL

---

## Lógica Detallada del Filtrado

### 1. Estructura de Datos para el Filtrado

```abap
TYPES: BEGIN OF ty_bukrs_range,
         sign   TYPE c LENGTH 1,    " I = Include, E = Exclude
         option TYPE c LENGTH 2,    " EQ, BT, etc.
         low    TYPE bukrs,         " Valor inferior del rango
         high   TYPE bukrs,         " Valor superior del rango
       END OF ty_bukrs_range.
```

**Explicación**: Esta estructura define un rango de selección compatible con SELECT-OPTIONS de SAP. Permite filtrar sociedades de manera flexible (rangos, valores individuales, exclusiones, etc.).

---

### 2. Variables Globales para el Filtrado

```abap
DATA: lt_bukrs_filter  TYPE TABLE OF ty_bukrs_range,  " Filtro final aplicado
      ls_bukrs_filter  TYPE ty_bukrs_range,           " Línea individual
      lt_bukrs_debug   TYPE TABLE OF bukrs,           " Set de depuración
```

**Explicación**:
- `lt_bukrs_filter`: Tabla que contiene todos los rangos de BUKRS a filtrar
- `ls_bukrs_filter`: Estructura de trabajo para construir cada rango
- `lt_bukrs_debug`: Conjunto predefinido de sociedades para depuración

---

### 3. Parámetros de Selección

```abap
SELECT-OPTIONS: s_bukrs FOR t001-bukrs.    " Rango de sociedades
PARAMETERS: p_debug TYPE flag DEFAULT ' '.  " Checkbox para modo depuración
```

**Explicación**:
- `s_bukrs`: Permite al usuario especificar rangos de sociedades en pantalla
- `p_debug`: Checkbox que activa el modo depuración con conjunto predefinido

---

### 4. Formulario PREPARE_BUKRS_FILTER (Código Principal)

Este es el **núcleo del filtrado de BUKRS**. Se encuentra en las líneas 130-192.

#### 4.1 Modo Depuración (p_debug = 'X')

**Ubicación**: Líneas 138-166

```abap
IF p_debug = 'X'.
  " Cargar conjunto predefinido de sociedades para testing
  APPEND '1000' TO lt_bukrs_debug.
  APPEND '2000' TO lt_bukrs_debug.
  APPEND '3000' TO lt_bukrs_debug.

  " Convertir a rangos SELECT-OPTIONS
  LOOP AT lt_bukrs_debug INTO DATA(lv_bukrs_debug).
    CLEAR ls_bukrs_filter.
    ls_bukrs_filter-sign   = 'I'.     " Inclusivo
    ls_bukrs_filter-option = 'EQ'.    " Igual a
    ls_bukrs_filter-low    = lv_bukrs_debug.
    ls_bukrs_filter-high   = ''.
    APPEND ls_bukrs_filter TO lt_bukrs_filter.
  ENDLOOP.
```

**Lógica Paso a Paso**:

1. **Carga del Set de Datos**: Se definen las sociedades 1000, 2000 y 3000 como conjunto de depuración
2. **Conversión a Rangos**: Cada sociedad se convierte en un registro con:
   - `sign = 'I'`: Inclusión (no exclusión)
   - `option = 'EQ'`: Igual a (equivalencia exacta)
   - `low = código de sociedad`: Valor a filtrar
3. **Resultado**: `lt_bukrs_filter` contiene 3 registros, uno por cada sociedad

**Ventajas**:
- Permite testing sin modificar parámetros de pantalla
- Conjunto controlado y reproducible
- Evita errores en producción durante pruebas

#### 4.2 Modo Normal (p_debug = ' ')

**Ubicación**: Líneas 167-180

```abap
ELSE.
  " Usar rangos de la pantalla de selección
  LOOP AT s_bukrs INTO DATA(ls_bukrs_screen).
    CLEAR ls_bukrs_filter.
    ls_bukrs_filter-sign   = ls_bukrs_screen-sign.
    ls_bukrs_filter-option = ls_bukrs_screen-option.
    ls_bukrs_filter-low    = ls_bukrs_screen-low.
    ls_bukrs_filter-high   = ls_bukrs_screen-high.
    APPEND ls_bukrs_filter TO lt_bukrs_filter.
  ENDLOOP.
ENDIF.
```

**Lógica Paso a Paso**:

1. **Lectura de Pantalla**: Lee los valores ingresados por el usuario en `s_bukrs`
2. **Copia Directa**: Cada línea de `s_bukrs` se copia a `lt_bukrs_filter`
3. **Preservación de Opciones**: Mantiene todas las características del rango (inclusiones, exclusiones, rangos BT, etc.)

**Ventajas**:
- Flexibilidad total para el usuario final
- Soporta rangos complejos (BT, NE, etc.)
- Compatible con todas las funcionalidades de SELECT-OPTIONS

---

### 5. Aplicación del Filtro en la Consulta SQL

**Ubicación**: Línea 227 del formulario `EXTRACT_FI_DATA`

```abap
SELECT b~bukrs
       b~belnr
       ...
  FROM bkpf AS b
  INNER JOIN bseg AS s
    ON ...
  WHERE b~bukrs IN lt_bukrs_filter    " ***** APLICACIÓN DEL FILTRO *****
    AND b~budat IN s_budat
    AND ...
```

**Explicación**:

La cláusula `WHERE b~bukrs IN lt_bukrs_filter` aplica el filtro preparado:

1. **Comparación Dinámica**: SAP evalúa cada registro contra todos los rangos en `lt_bukrs_filter`
2. **Lógica de Inclusión/Exclusión**: Procesa automáticamente los valores de `sign` y `option`
3. **Optimización**: El motor de base de datos optimiza la consulta según los rangos

**Ejemplos de Funcionamiento**:

- Si `lt_bukrs_filter` contiene `EQ 1000`, filtra solo BUKRS = '1000'
- Si contiene `BT 1000-2000`, filtra BUKRS entre '1000' y '2000'
- Si contiene múltiples registros, aplica OR lógico entre ellos

---

## Diagrama de Flujo

```
START
  |
  v
¿p_debug = 'X'? ----NO----> Copiar s_bukrs[] a lt_bukrs_filter[]
  |                                   |
  YES                                 |
  |                                   |
  v                                   |
Cargar Set Depuración                 |
  (1000, 2000, 3000)                  |
  |                                   |
  v                                   |
Convertir a Rangos                    |
  (sign='I', option='EQ')             |
  |                                   |
  v                                   v
  +-----------------------------------+
  |
  v
Validar lt_bukrs_filter no vacío
  |
  v
Aplicar en WHERE de SELECT
  |
  v
END
```

---

## Ejemplos de Uso

### Ejemplo 1: Modo Depuración

**Entrada**:
- Marcar checkbox `p_debug`

**Resultado**:
- Se filtran automáticamente las sociedades 1000, 2000, 3000
- No importa lo que esté en `s_bukrs`

### Ejemplo 2: Modo Normal - Sociedad Individual

**Entrada**:
- `s_bukrs`: EQ 1000

**Resultado**:
- Solo se extraen datos de la sociedad 1000

### Ejemplo 3: Modo Normal - Rango

**Entrada**:
- `s_bukrs`: BT 1000 - 5000

**Resultado**:
- Se extraen datos de sociedades entre 1000 y 5000

### Ejemplo 4: Modo Normal - Múltiples Valores

**Entrada**:
- `s_bukrs`: 
  - Línea 1: EQ 1000
  - Línea 2: EQ 3000
  - Línea 3: EQ 5000

**Resultado**:
- Se extraen datos de las sociedades 1000, 3000 y 5000

---

## Ventajas de esta Implementación

1. **Depuración Facilitada**: El modo debug permite pruebas rápidas sin modificar pantallas
2. **Flexibilidad**: Soporta cualquier combinación de rangos de SELECT-OPTIONS
3. **Reutilizable**: La tabla `lt_bukrs_filter` puede usarse en múltiples consultas
4. **Mantenible**: El conjunto de depuración se modifica fácilmente
5. **Seguro**: Validación de que siempre hay al menos un filtro
6. **Documentado**: Código autoexplicativo con comentarios detallados

---

## Modificaciones Futuras

Para agregar más sociedades de depuración:

```abap
" En el formulario PREPARE_BUKRS_FILTER, líneas 143-149
APPEND '1000' TO lt_bukrs_debug.
APPEND '2000' TO lt_bukrs_debug.
APPEND '3000' TO lt_bukrs_debug.
APPEND '4000' TO lt_bukrs_debug.  " <-- Nueva sociedad
APPEND '5000' TO lt_bukrs_debug.  " <-- Nueva sociedad
```

Para cambiar la lógica de filtrado:
- Modificar el formulario `PREPARE_BUKRS_FILTER` (líneas 130-192)
- Mantener la estructura de salida en `lt_bukrs_filter`

---

## Conclusión

El código de filtrado de BUKRS está implementado de manera modular y extensible. La ubicación principal es el formulario `PREPARE_BUKRS_FILTER` (líneas 130-192), con aplicación en la línea 227. La lógica permite tanto depuración controlada como uso flexible en producción.
