--- Retrieves a list of all customers in the ne_comp table
DATA_QRY_BLOCK_BEGIN GetCustomerPhoneNumbers:
	SELECT 	DISTINCT nc.char_parm33, n.name, n.id
	FROM 	ne n,
			ne_comp nc 
	JOIN 	ne_comp_type nct 
			ON  nc.ne_comp_type_id = nct.id AND 
				nct.name LIKE 'CUST LINE'
	WHERE 	nc.ne_id = n.id AND
			nc.char_parm33 IS NOT NULL
			--AND nc.char_parm33 = '201/265/8131'
			--AND nc.char_parm33 = '3609888222'
	-- Created this index:   CREATE INDEX nc_cp33_name_id_idx ON ne_comp (char_parm33, name, id); 
	-- REMEMBER TO DROP IT!
DATA_QRY_BLOCK_END

-- Get DSLAM name from Customer ID
DATA_QRY_BLOCK_BEGIN GetDslamFromCustomerID:
	SELECT  DISTINCT n.name, n.id
	FROM 	ne n,
			ne_comp nc JOIN ne_comp_type nct 
						ON nc.ne_comp_type_id = nct.id AND 
							nct.name LIKE 'CUST LINE'
	WHERE 	nc.ne_id = n.id AND
			nc.char_parm33 IS NOT NULL AND
			nc.char_parm33 = ?
DATA_QRY_BLOCK_END

-- The following block retrieves the comp type ids of the comp types being used
DATA_QRY_BLOCK_BEGIN GetCompTypeIds:
	SELECT id 
	FROM ne_comp_type 
	WHERE name LIKE 'CBT OPS%'AND
		  name NOT LIKE '%KPI%'
DATA_QRY_BLOCK_END

-- Adds rows to the NE_COMP table
DATA_QRY_BLOCK_BEGIN AddNEComp:
    INSERT INTO ne_comp (ID, NAME, NE_COMP_TYPE_ID, NE_ID)
	VALUES (SEQ_NE_COMP.nextval, ?, ?, ?) 	
DATA_QRY_BLOCK_END

-- Retrieves the id of the cbt ops customer pm study model
DATA_QRY_BLOCK_BEGIN GetStudyModel:
	SELECT id FROM study_model WHERE name = 'cbt_ops_cust_pm'
DATA_QRY_BLOCK_END

-- Retrive the id of the last collector added
DATA_QRY_BLOCK_BEGIN GetCurrCollectorID:
	SELECT SEQ_COLLECTOR.currval FROM DBA_SEQUENCES
DATA_QRY_BLOCK_END

-- Check that a collector is already in the table
DATA_QRY_BLOCK_BEGIN CollectorExists:
	SELECT 	COUNT(*)
	FROM 	collector
	WHERE	name = ?
DATA_QRY_BLOCK_END

-- Add a collector
DATA_QRY_BLOCK_BEGIN AddCollector:
	INSERT INTO collector (ID, NE_ID, NAME, TYPE, ACTIVE, INTERVAL, RPU_HOSTNAME, DESCRIPTION)
	VALUES (SEQ_COLLECTOR.nextval, ?, ?, 'dedicated', 'N', 3600, ?, 'Customer Statistics Collection')
DATA_QRY_BLOCK_END

-- Add a study
DATA_QRY_BLOCK_BEGIN AddStudy:
	INSERT INTO study 
	(ID, NAME, STUDY_MODEL_ID, NE_ID, DESCRIPTION, ACTIVE, COLLECTOR_ID, PERIOD_SIZE, PROCESS_RESULTS_REPORT, CONSEC_BUSY_SEASON_DAYS, CONSEC_BUSY_SEASON_WEEKS, CONSEC_BUSY_SEASON_MONTHS) 
	VALUES (SEQ_STUDY.nextval, ?, ?, ?, 'Customer PM Statistics Collection', 'Y', SEQ_COLLECTOR.currval, 900, 'SUMMARY,TYPES,SECTIONS,DETAILS,COMPREHENSIVE,MISSING,SUSPECT,EXCEPTIONS', 'Y', 'Y', 'Y')
DATA_QRY_BLOCK_END

-- Add a dedicated
DATA_QRY_BLOCK_BEGIN AddDedicated:
	INSERT INTO dedicated (ID, COLLECTOR_ID) VALUES (SEQ_DEDICATED.nextval, SEQ_COLLECTOR.currval)
DATA_QRY_BLOCK_END
