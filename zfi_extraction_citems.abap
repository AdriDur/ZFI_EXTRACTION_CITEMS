*&---------------------------------------------------------------------*
*& Report ZFI_EXTRACTION_CITEMS
*&---------------------------------------------------------------------*
*& Programa para extracción de ítems contables de FI con filtrado de
*& sociedades (BUKRS) mediante conjunto de datos para depuración
*&---------------------------------------------------------------------*
REPORT zfi_extraction_citems.

*&---------------------------------------------------------------------*
*& TABLAS Y TIPOS DE DATOS
*&---------------------------------------------------------------------*
TABLES: bkpf,    " Cabecera de documento contable
        bseg,    " Segmento de documento contable
        t001.    " Sociedades

*&---------------------------------------------------------------------*
*& TIPOS PERSONALIZADOS
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_bukrs_range,
         sign   TYPE c LENGTH 1,
         option TYPE c LENGTH 2,
         low    TYPE bukrs,
         high   TYPE bukrs,
       END OF ty_bukrs_range.

TYPES: BEGIN OF ty_extraction_data,
         bukrs TYPE bukrs,         " Sociedad
         belnr TYPE belnr_d,       " Número de documento
         gjahr TYPE gjahr,         " Ejercicio
         buzei TYPE buzei,         " Número de posición
         budat TYPE budat,         " Fecha de contabilización
         blart TYPE blart,         " Clase de documento
         waers TYPE waers,         " Moneda
         hkont TYPE hkont,         " Cuenta de mayor
         wrbtr TYPE wrbtr,         " Importe
         sgtxt TYPE sgtxt,         " Texto
       END OF ty_extraction_data.

*&---------------------------------------------------------------------*
*& DATOS INTERNOS
*&---------------------------------------------------------------------*
DATA: lt_bukrs_filter  TYPE TABLE OF ty_bukrs_range,
      ls_bukrs_filter  TYPE ty_bukrs_range,
      lt_bukrs_debug   TYPE TABLE OF bukrs,
      lt_extraction    TYPE TABLE OF ty_extraction_data,
      ls_extraction    TYPE ty_extraction_data.

*&---------------------------------------------------------------------*
*& PANTALLA DE SELECCIÓN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

* Parámetros para filtrado de sociedades
SELECT-OPTIONS: s_bukrs FOR t001-bukrs.    " Rango de sociedades

* Parámetros de fecha
SELECT-OPTIONS: s_budat FOR bkpf-budat.    " Fecha de contabilización
SELECT-OPTIONS: s_gjahr FOR bkpf-gjahr.    " Ejercicio

* Parámetros adicionales
SELECT-OPTIONS: s_blart FOR bkpf-blart.    " Clase de documento

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.

* Checkbox para activar modo depuración con conjunto predefinido
PARAMETERS: p_debug TYPE flag DEFAULT ' '.

SELECTION-SCREEN END OF BLOCK b2.

*&---------------------------------------------------------------------*
*& TEXTOS DE SELECCIÓN
*&---------------------------------------------------------------------*
* TEXT-001: 'Parámetros de Selección'
* TEXT-002: 'Modo Depuración'

*&---------------------------------------------------------------------*
*& INICIALIZACIÓN
*&---------------------------------------------------------------------*
INITIALIZATION.
  " Valores por defecto para facilitar testing
  s_budat-sign   = 'I'.
  s_budat-option = 'BT'.
  s_budat-low    = sy-datum - 365.
  s_budat-high   = sy-datum.
  APPEND s_budat.

*&---------------------------------------------------------------------*
*& AT SELECTION-SCREEN
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
  " Validación de parámetros
  IF s_bukrs[] IS INITIAL AND p_debug IS INITIAL.
    MESSAGE 'Debe especificar al menos una sociedad o activar modo depuración' TYPE 'E'.
  ENDIF.

*&---------------------------------------------------------------------*
*& START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM prepare_bukrs_filter.
  PERFORM extract_fi_data.
  PERFORM display_results.

*&---------------------------------------------------------------------*
*& END-OF-SELECTION
*&---------------------------------------------------------------------*
END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  PREPARE_BUKRS_FILTER
*&---------------------------------------------------------------------*
*       Prepara el filtro de sociedades (BUKRS) para la extracción
*----------------------------------------------------------------------*
* LÓGICA DETALLADA:
* 1. Si el modo depuración está activo (p_debug = 'X'):
*    - Se carga un conjunto predefinido de sociedades para testing
*    - Este conjunto permite probar el programa sin afectar datos reales
*    - Las sociedades de depuración se convierten a rangos SELECT-OPTIONS
*
* 2. Si el modo depuración NO está activo:
*    - Se utilizan las sociedades especificadas en la pantalla (s_bukrs)
*    - Esto permite flexibilidad en producción
*
* 3. El resultado es una tabla de rangos (lt_bukrs_filter) que se usa
*    en la cláusula WHERE de las consultas SQL posteriores
*----------------------------------------------------------------------*
FORM prepare_bukrs_filter.

  CLEAR: lt_bukrs_filter[], lt_bukrs_debug[].

  " ***** INICIO DEL CÓDIGO DE FILTRADO DE BUKRS *****
  " UBICACIÓN: Formulario prepare_bukrs_filter
  " LÍNEAS: Esta sección implementa la lógica de filtrado

  IF p_debug = 'X'.
    " Modo depuración: Usar conjunto predefinido de sociedades
    " Este conjunto se puede modificar según necesidades de testing

    " Sociedad 1000 - Ejemplo para ambiente de pruebas
    APPEND '1000' TO lt_bukrs_debug.

    " Sociedad 2000 - Ejemplo para ambiente de pruebas
    APPEND '2000' TO lt_bukrs_debug.

    " Sociedad 3000 - Ejemplo para ambiente de pruebas
    APPEND '3000' TO lt_bukrs_debug.

    " Convertir el conjunto de datos de depuración a rangos
    LOOP AT lt_bukrs_debug INTO DATA(lv_bukrs_debug).
      CLEAR ls_bukrs_filter.
      ls_bukrs_filter-sign   = 'I'.     " Inclusivo
      ls_bukrs_filter-option = 'EQ'.    " Igual a
      ls_bukrs_filter-low    = lv_bukrs_debug.
      ls_bukrs_filter-high   = ''.
      APPEND ls_bukrs_filter TO lt_bukrs_filter.
    ENDLOOP.

    WRITE: / 'Modo depuración activado'.
    WRITE: / 'Sociedades filtradas:', sy-tabix, 'registros'.
    LOOP AT lt_bukrs_debug INTO lv_bukrs_debug.
      WRITE: / '  - BUKRS:', lv_bukrs_debug.
    ENDLOOP.

  ELSE.
    " Modo normal: Usar rangos de la pantalla de selección
    " Copiar los valores de s_bukrs a la tabla de filtros

    LOOP AT s_bukrs INTO DATA(ls_bukrs_screen).
      CLEAR ls_bukrs_filter.
      ls_bukrs_filter-sign   = ls_bukrs_screen-sign.
      ls_bukrs_filter-option = ls_bukrs_screen-option.
      ls_bukrs_filter-low    = ls_bukrs_screen-low.
      ls_bukrs_filter-high   = ls_bukrs_screen-high.
      APPEND ls_bukrs_filter TO lt_bukrs_filter.
    ENDLOOP.

    WRITE: / 'Modo normal activado'.
    WRITE: / 'Usando rangos especificados en pantalla de selección'.

  ENDIF.

  " ***** FIN DEL CÓDIGO DE FILTRADO DE BUKRS *****

  " Validar que tenemos al menos un filtro
  IF lt_bukrs_filter[] IS INITIAL.
    MESSAGE 'No se pudo construir el filtro de sociedades' TYPE 'E'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  EXTRACT_FI_DATA
*&---------------------------------------------------------------------*
*       Extrae los datos de FI usando el filtro de BUKRS preparado
*----------------------------------------------------------------------*
* LÓGICA:
* - Realiza JOIN entre BKPF (cabecera) y BSEG (posiciones)
* - Aplica el filtro de sociedades (lt_bukrs_filter) en la cláusula WHERE
* - Aplica filtros adicionales de fecha, ejercicio y clase de documento
* - Limita los resultados a 1000 registros para evitar sobrecargas
*----------------------------------------------------------------------*
FORM extract_fi_data.

  CLEAR lt_extraction[].

  " Extraer datos aplicando el filtro de BUKRS
  SELECT b~bukrs
         b~belnr
         b~gjahr
         s~buzei
         b~budat
         b~blart
         b~waers
         s~hkont
         s~wrbtr
         s~sgtxt
    INTO CORRESPONDING FIELDS OF TABLE lt_extraction
    UP TO 1000 ROWS
    FROM bkpf AS b
    INNER JOIN bseg AS s
      ON b~bukrs = s~bukrs
     AND b~belnr = s~belnr
     AND b~gjahr = s~gjahr
    WHERE b~bukrs IN lt_bukrs_filter    " ***** APLICACIÓN DEL FILTRO BUKRS *****
      AND b~budat IN s_budat
      AND b~gjahr IN s_gjahr
      AND b~blart IN s_blart.

  IF sy-subrc = 0.
    WRITE: / 'Registros extraídos:', sy-dbcnt.
  ELSE.
    WRITE: / 'No se encontraron registros con los criterios especificados'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_RESULTS
*&---------------------------------------------------------------------*
*       Muestra los resultados de la extracción
*----------------------------------------------------------------------*
FORM display_results.

  IF lt_extraction[] IS NOT INITIAL.

    WRITE: / sy-uline(120).
    WRITE: / 'Resumen de Extracción de Items Contables'.
    WRITE: / sy-uline(120).
    SKIP.

    WRITE: / 'BUKRS', 10 'Documento', 25 'Ejercicio', 35 'Pos',
             40 'F.Contab.', 52 'Clase', 60 'Cuenta', 75 'Importe',
             95 'Moneda'.
    WRITE: / sy-uline(120).

    LOOP AT lt_extraction INTO ls_extraction.
      WRITE: / ls_extraction-bukrs,
               10 ls_extraction-belnr,
               25 ls_extraction-gjahr,
               35 ls_extraction-buzei,
               40 ls_extraction-budat,
               52 ls_extraction-blart,
               60 ls_extraction-hkont,
               75 ls_extraction-wrbtr,
               95 ls_extraction-waers.

      " Limitar a primeros 100 para evitar overflow en pantalla
      IF sy-tabix >= 100.
        SKIP.
        WRITE: / '... (mostrando primeros 100 registros)'.
        EXIT.
      ENDIF.
    ENDLOOP.

    WRITE: / sy-uline(120).
    WRITE: / 'Total de registros:', lines( lt_extraction ).

  ELSE.
    SKIP.
    WRITE: / 'No hay datos para mostrar'.
  ENDIF.

ENDFORM.
