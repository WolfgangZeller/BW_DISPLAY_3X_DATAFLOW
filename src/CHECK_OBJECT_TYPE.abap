METHOD.

SELECT 'ADSO' AS type
FROM rsoadso
WHERE objvers = 'A'
AND adsonm = @i_target_provider
INTO @r_type.
ENDSELECT.

SELECT 'ODSO' AS type
FROM rsdodso
WHERE objvers = 'A'
AND odsobject = @i_target_provider
INTO @r_type.
ENDSELECT.

SELECT 'IOBJ' AS type
FROM rsdiobj
WHERE objvers = 'A'
AND iobjnm = @i_target_provider
INTO @r_type.
ENDSELECT.

SELECT 'CUBE' AS type
FROM rsdcube
WHERE objvers = 'A'
AND infocube = @i_target_provider
INTO @r_type.
ENDSELECT.

ENDMETHOD.
