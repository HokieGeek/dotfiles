DATA_QRY_BLOCK_BEGIN GetCurrNESequence:
	SELECT SEQ_NE.currval FROM DBA_SEQUENCES
DATA_QRY_BLOCK_END

DATA_QRY_BLOCK_BEGIN GetCurrNETypeSequence:
	SELECT SEQ_NE_TYPE.currval FROM DBA_SEQUENCES
DATA_QRY_BLOCK_END

-- The following block retrieves the comp type ids of the comp types being used
DATA_QRY_BLOCK_BEGIN GetNECompTypes:
	SELECT name 
	FROM ne_comp_type 
	WHERE name LIKE 'CBT OPS%'AND
		  name NOT LIKE '%KPI%' AND
		  name NOT LIKE '%CUST'
DATA_QRY_BLOCK_END

-- Retreive a list of all dslam names
DATA_QRY_BLOCK_BEGIN GetCustomers2:
	SELECT 	nc.name, n.region, n.state, n.lata, n.location, n.name
	FROM 	ne n, 
			ne_type nt JOIN ne_type_version ntv
							ON 	ntv.NE_TYPE_ID = nt.ID,
			ne_comp nc JOIN ne_comp_type nct 
							ON nc.ne_comp_type_id = nct.id
							AND nct.name LIKE 'CBT OPS CUST'
	WHERE 	nc.ne_id = n.id AND
			ntv.id = n.ne_type_version_id AND
			nc.name IN ('798-799-9898',  '240-404-3136', '798-783-8069')
			--nc.name = '201-200-0101'
DATA_QRY_BLOCK_END
DATA_QRY_BLOCK_BEGIN GetCustomers:
	SELECT 	nc.name, n.region, n.state, n.lata, n.location, n.name
	FROM 	ne n, 
			ne_type nt JOIN ne_type_version ntv
							ON 	ntv.NE_TYPE_ID = nt.ID,
			ne_comp nc JOIN ne_comp_type nct 
							ON nc.ne_comp_type_id = nct.id
							AND nct.name LIKE 'CBT OPS CUST'
	WHERE 	nc.ne_id = n.id AND
			ntv.id = n.ne_type_version_id
DATA_QRY_BLOCK_END

-- ID = some number?
-- MODEL = Virtual
-- TYPE = Region/State/District/Lata/Location
-- DESCRIPTION = just.. whatever
DATA_QRY_BLOCK_BEGIN AddVirtualType:
	INSERT INTO ne_type (ID, VENDOR, MODEL, TYPE, DESCRIPTION) VALUES (SEQ_NE_TYPE.nextval, 'JDSU', 'Virtual', ?, 'A Virtual NE is a pseudo network element.')
DATA_QRY_BLOCK_END

-- TIP: ne_type.ID = ne_type_version.ID
DATA_QRY_BLOCK_BEGIN AddVirtualTypeVersion:
	INSERT INTO ne_type_version (ID, NE_TYPE_ID, VERSION) VALUES (SEQ_NE_TYPE_VERSION.nextval, ?, 'Any')
DATA_QRY_BLOCK_END

-- DESCRIPTION = just.. whatever
DATA_QRY_BLOCK_BEGIN RemoveVirtualType:
	DELETE FROM ne_type WHERE ID LIKE ?
DATA_QRY_BLOCK_END

-- TIP: ne_type.ID = ne_type_version.ID
DATA_QRY_BLOCK_BEGIN RemoveVirtualTypeVersion:
	DELETE FROM ne_type_version WHERE ID LIKE ?
DATA_QRY_BLOCK_END

-- Determine the NE_TYPE id for a specific Virtual NE type
DATA_QRY_BLOCK_BEGIN GetVirtualTypeID:
	SELECT 	ntv.ID
	FROM	ne_type_version ntv,
			ne_type nt
	WHERE	nt.ID = ntv.NE_TYPE_ID AND
			nt.VENDOR = 'JDSU' AND
			nt.MODEL = 'Virtual' AND
			nt.TYPE = ?
DATA_QRY_BLOCK_END

-- Retrive the KPI SUM NE COMP TYPE ID
DATA_QRY_BLOCK_BEGIN GetCompTypeKPIID:
	SELECT id
	FROM ne_comp_type 
	WHERE name = 'CBT OPS KPI SUM'
DATA_QRY_BLOCK_END

-- What about the region, state, district, lata and location columns?
-- 1-4 (WAN), 5-8 (LAN), 9-12 (IPTV)

-- ****************************************

DATA_QRY_BLOCK_BEGIN AddVirtualNERegion:
	INSERT INTO ne (ID, NAME, NE_TYPE_VERSION_ID, REGION) 
	VALUES (SEQ_NE.nextval, ?, ?, ?)
DATA_QRY_BLOCK_END

-- 
DATA_QRY_BLOCK_BEGIN AddVirtualNEState:
	INSERT INTO ne (ID, NAME, NE_TYPE_VERSION_ID, REGION, STATE) 
	VALUES (SEQ_NE.nextval, ?, ?, ?, ?)
DATA_QRY_BLOCK_END

-- 
DATA_QRY_BLOCK_BEGIN AddVirtualNEDistrict:
	INSERT INTO ne (ID, NAME, NE_TYPE_VERSION_ID, REGION, STATE, DISTRICT) 
	VALUES (SEQ_NE.nextval, ?, ?, ?, ?, ?)
DATA_QRY_BLOCK_END

-- 
DATA_QRY_BLOCK_BEGIN AddVirtualNELata:
	INSERT INTO ne (ID, NAME, NE_TYPE_VERSION_ID, REGION, STATE, LATA) 
	VALUES (SEQ_NE.nextval, ?, ?, ?, ?, ?)
DATA_QRY_BLOCK_END

-- 
DATA_QRY_BLOCK_BEGIN AddVirtualNELocation:
	INSERT INTO ne (ID, NAME, NE_TYPE_VERSION_ID, REGION, STATE, LATA, LOCATION) 
	VALUES (SEQ_NE.nextval, ?, ?, ?, ?, ?, ?)
DATA_QRY_BLOCK_END

-- 
DATA_QRY_BLOCK_BEGIN GetDSLAMID:
	SELECT 	DISTINCT n.ID
	FROM	ne n,
			ne_type nt JOIN ne_type_version ntv
						ON 	ntv.NE_TYPE_ID = nt.ID
	WHERE	n.REGION = ? AND
			n.STATE = ? AND
			n.LATA = ? AND
			n.LOCATION = ? AND
			n.NAME = ?
DATA_QRY_BLOCK_END

-- 
DATA_QRY_BLOCK_BEGIN AddKPIValues:
    INSERT INTO ne_comp
	(ID, NAME, NE_COMP_TYPE_ID, NE_ID, NUM_PARM1, NUM_PARM2, NUM_PARM3, NUM_PARM4, NUM_PARM5, NUM_PARM6, NUM_PARM7, NUM_PARM8, NUM_PARM9, NUM_PARM10, NUM_PARM11, NUM_PARM12)
	--(ID, NAME, NE_COMP_TYPE_ID, NE_ID, NUM_PARM2, NUM_PARM1, NUM_PARM3, NUM_PARM4, NUM_PARM6, NUM_PARM5, NUM_PARM7, NUM_PARM8, NUM_PARM10, NUM_PARM9, NUM_PARM11, NUM_PARM12)
	VALUES (SEQ_NE_COMP.nextval, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) 	
DATA_QRY_BLOCK_END