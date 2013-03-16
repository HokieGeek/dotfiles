DATA_QRY_BLOCK_BEGIN GetCusts:
	SELECT 	nc.name,  n.name
	FROM 	ne n, 
			ne_type nt JOIN ne_type_version ntv
							ON 	ntv.NE_TYPE_ID = nt.ID,
			ne_comp nc JOIN ne_comp_type nct 
							ON nc.ne_comp_type_id = nct.id
							AND nct.name LIKE 'CBT OPS CUST'
	WHERE 	nc.ne_id = n.id AND
			ntv.id = n.ne_type_version_id
	GROUP BY nc.name, n.name
DATA_QRY_BLOCK_END

DATA_QRY_BLOCK_BEGIN GetCusts2:
	SELECT 	nc.name
	FROM 	ne_comp nc JOIN ne_comp_type nct 
						ON nc.ne_comp_type_id = nct.id
						AND nct.name LIKE 'CBT OPS CUST'
DATA_QRY_BLOCK_END

DATA_QRY_BLOCK_BEGIN GetCompTypeIds:
	SELECT 	id, name
	FROM	ne_comp_type
	WHERE	name LIKE 'CBT OPS RG%' OR
			name LIKE 'CBT OPS STB%'
DATA_QRY_BLOCK_END

DATA_QRY_BLOCK_BEGIN AddEquipment:
	-- when using, add line: AND name IN
	UPDATE 	ne_comp
	SET		char_parm3 = ?, 
			char_parm4 = ?,
			char_parm5 = ?,
			char_parm6 = ?,
			char_parm7 = ?
	WHERE	ne_comp_type_id IN 
DATA_QRY_BLOCK_END

DATA_QRY_BLOCK_BEGIN AddEquipment2:
	-- when using, add line: AND name IN
	UPDATE 	ne_comp
	SET		char_parm3 = ?, 
			char_parm4 = ?,
			char_parm5 = ?
	WHERE	ne_comp_type_id IN 
DATA_QRY_BLOCK_END


DATA_QRY_BLOCK_BEGIN Clear:
	UPDATE 	ne_comp
	SET		char_parm3 = null, 
			char_parm4 = null,
			char_parm5 = null,
			char_parm6 = null,
			char_parm7 = null
	WHERE	ne_comp_type_id IN (SELECT id FROM	ne_comp_type WHERE	name LIKE 'CBT OPS RG%' OR name LIKE 'CBT OPS STB%') 
			AND name IN  (SELECT nc.name FROM ne_comp nc JOIN ne_comp_type nct ON nc.ne_comp_type_id = nct.id AND nct.name LIKE 'CBT OPS CUST'
			)
DATA_QRY_BLOCK_END