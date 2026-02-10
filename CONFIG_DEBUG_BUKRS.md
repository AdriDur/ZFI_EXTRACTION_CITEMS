# Configuración del Set de Datos para Depuración de BUKRS
# Este archivo documenta las sociedades utilizadas en modo debug

## Sociedades de Depuración Activas

Las siguientes sociedades (BUKRS) están configuradas en el modo depuración del programa:

| BUKRS | Descripción                    | Propósito                      |
|-------|--------------------------------|--------------------------------|
| 1000  | Sociedad de Pruebas 1         | Testing básico                 |
| 2000  | Sociedad de Pruebas 2         | Testing con múltiples módulos  |
| 3000  | Sociedad de Pruebas 3         | Testing de integración         |

## Ubicación en el Código

Archivo: `zfi_extraction_citems.abap`
Formulario: `PREPARE_BUKRS_FILTER`
Líneas: 143-149

## Instrucciones para Modificar el Set de Depuración

Para agregar o modificar sociedades de depuración:

1. Abrir el archivo `zfi_extraction_citems.abap`
2. Localizar el formulario `PREPARE_BUKRS_FILTER`
3. En la sección del modo depuración (líneas 138-166)
4. Modificar las líneas APPEND:

```abap
IF p_debug = 'X'.
  " Agregar las sociedades deseadas
  APPEND '1000' TO lt_bukrs_debug.
  APPEND '2000' TO lt_bukrs_debug.
  APPEND '3000' TO lt_bukrs_debug.
  " Agregar más sociedades según necesidad:
  " APPEND '4000' TO lt_bukrs_debug.
  " APPEND '5000' TO lt_bukrs_debug.
  ...
```

## Ejemplos de Configuraciones Alternativas

### Configuración para Testing Unitario
```abap
APPEND 'T001' TO lt_bukrs_debug.  " Sociedad test 1
```

### Configuración para Testing de Integración
```abap
APPEND 'DEV1' TO lt_bukrs_debug.  " Desarrollo 1
APPEND 'DEV2' TO lt_bukrs_debug.  " Desarrollo 2
APPEND 'QAS1' TO lt_bukrs_debug.  " Quality Assurance
```

### Configuración para Pruebas Específicas de País
```abap
APPEND 'MX01' TO lt_bukrs_debug.  " México
APPEND 'US01' TO lt_bukrs_debug.  " Estados Unidos
APPEND 'BR01' TO lt_bukrs_debug.  " Brasil
```

## Notas Importantes

1. El modo depuración se activa marcando el checkbox `p_debug` en la pantalla de selección
2. Cuando el modo debug está activo, los valores de `s_bukrs` son ignorados
3. El set de depuración está diseñado para ambientes de desarrollo/pruebas
4. NO usar sociedades productivas en el set de depuración
5. Mantener el set pequeño (3-5 sociedades) para facilitar el testing

## Validación del Set de Depuración

Antes de ejecutar en modo debug, verificar que:
- Las sociedades existen en la tabla T001
- El usuario tiene autorización para las sociedades
- Las sociedades contienen datos de prueba adecuados

## Historial de Cambios

| Fecha      | Versión | Cambios                              |
|------------|---------|--------------------------------------|
| 2026-02-10 | 1.0     | Implementación inicial con 1000, 2000, 3000 |
