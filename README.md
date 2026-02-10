# ZFI_EXTRACTION_CITEMS

## Descripción

Programa ABAP para la extracción de ítems contables de FI (Financial) con funcionalidad avanzada de filtrado de sociedades (BUKRS) mediante conjunto de datos predefinido para depuración.

## Características Principales

### 1. Filtrado Flexible de Sociedades (BUKRS)
- Soporte para rangos de selección (SELECT-OPTIONS)
- Modo depuración con conjunto predefinido de sociedades
- Validación automática de parámetros

### 2. Modo Depuración
- Checkbox `p_debug` para activar conjunto predefinido
- Set de datos configurable (actualmente: 1000, 2000, 3000)
- Ideal para testing sin afectar datos productivos

### 3. Parámetros de Selección
- Rango de sociedades (s_bukrs)
- Fecha de contabilización (s_budat)
- Ejercicio (s_gjahr)
- Clase de documento (s_blart)

## Archivos del Proyecto

| Archivo | Descripción |
|---------|-------------|
| `zfi_extraction_citems.abap` | Programa principal ABAP |
| `DOCUMENTACION_FILTRADO_BUKRS.md` | Documentación detallada del filtrado |
| `CONFIG_DEBUG_BUKRS.md` | Configuración del set de depuración |
| `README.md` | Este archivo |

## Documentación Detallada

### Ubicación del Código de Filtrado

El código de filtrado de BUKRS se encuentra en:
- **Archivo**: `zfi_extraction_citems.abap`
- **Formulario**: `PREPARE_BUKRS_FILTER` (líneas 105-169)
- **Aplicación**: Línea 189 en la consulta SQL

Ver [DOCUMENTACION_FILTRADO_BUKRS.md](DOCUMENTACION_FILTRADO_BUKRS.md) para detalles completos.

## Uso Rápido

### Modo Normal
1. Ejecutar el programa ZFI_EXTRACTION_CITEMS
2. Ingresar sociedades en el campo `s_bukrs`
3. Completar otros parámetros opcionales
4. Ejecutar (F8)

### Modo Depuración
1. Ejecutar el programa ZFI_EXTRACTION_CITEMS
2. Marcar checkbox `Modo Depuración`
3. Ejecutar (F8)
4. El programa usará automáticamente las sociedades: 1000, 2000, 3000

## Estructura del Código

```
zfi_extraction_citems.abap
├── Declaraciones (tipos, tablas, datos)
├── Pantalla de Selección
│   ├── Block 1: Parámetros principales
│   └── Block 2: Modo depuración
├── INITIALIZATION
├── AT SELECTION-SCREEN (validaciones)
├── START-OF-SELECTION
│   ├── PREPARE_BUKRS_FILTER ← Filtrado de BUKRS
│   ├── EXTRACT_FI_DATA      ← Extracción con filtro
│   └── DISPLAY_RESULTS      ← Visualización
└── Formularios (FORMs)
```

## Lógica de Filtrado - Resumen

### Flujo del Filtrado de BUKRS

1. **Preparación del Filtro** (`PREPARE_BUKRS_FILTER`):
   - Si `p_debug = 'X'`: Cargar set predefinido (1000, 2000, 3000)
   - Si `p_debug = ' '`: Usar valores de pantalla (`s_bukrs`)
   - Convertir a tabla de rangos (`lt_bukrs_filter`)

2. **Aplicación del Filtro** (`EXTRACT_FI_DATA`):
   - Usar `WHERE bukrs IN lt_bukrs_filter`
   - Combinar con otros filtros (fecha, ejercicio, etc.)

3. **Validación**:
   - Verificar que `lt_bukrs_filter` no esté vacío
   - Mostrar mensajes informativos

## Modificación del Set de Depuración

Para cambiar las sociedades de depuración:

1. Editar `zfi_extraction_citems.abap`
2. Ir al formulario `PREPARE_BUKRS_FILTER` (líneas 124-129)
3. Modificar los APPEND según necesidad:

```abap
APPEND '1000' TO lt_bukrs_debug.  " Modificar código
APPEND '2000' TO lt_bukrs_debug.  " Modificar código
APPEND '3000' TO lt_bukrs_debug.  " Modificar código
" APPEND 'XXXX' TO lt_bukrs_debug.  " Agregar más
```

Ver [CONFIG_DEBUG_BUKRS.md](CONFIG_DEBUG_BUKRS.md) para más detalles.

## Ventajas de la Implementación

✅ **Modular**: Lógica de filtrado separada en formulario dedicado  
✅ **Flexible**: Soporta rangos complejos de SELECT-OPTIONS  
✅ **Depurable**: Modo debug con set predefinido  
✅ **Documentado**: Comentarios detallados en el código  
✅ **Mantenible**: Fácil modificación del set de depuración  
✅ **Seguro**: Validaciones de parámetros obligatorios  

## Próximos Pasos

- [ ] Testing en ambiente de desarrollo
- [ ] Validación con datos reales
- [ ] Ajuste del set de depuración según necesidades
- [ ] Implementación de variantes de selección
- [ ] Integración con herramientas de extracción

## Soporte

Para preguntas o problemas:
1. Revisar la documentación en `DOCUMENTACION_FILTRADO_BUKRS.md`
2. Verificar la configuración en `CONFIG_DEBUG_BUKRS.md`
3. Revisar los comentarios en el código fuente

## Autor

Implementado para depuración y extracción de sociedades FI.

---

**Última actualización**: 2026-02-10  
**Versión**: 1.0