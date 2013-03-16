-- 
DATA_QRY_BLOCK_BEGIN DelMeasurements:
DELETE FROM measurement WHERE component_id IN (
SELECT id FROM component WHERE ne_comp_id IN (SELECT id FROM ne_comp 
WHERE ne_comp_type_id IN (SELECT id FROM ne_comp_type WHERE name LIKE 'CBT OPS%' AND name NOT LIKE '%KPI SUM') AND name != '240-404-3136' AND name != '201-200-0101')
)
DATA_QRY_BLOCK_END

-- 
DATA_QRY_BLOCK_BEGIN DelComponents:
DELETE FROM component WHERE ne_comp_id IN (SELECT id FROM ne_comp 
WHERE ne_comp_type_id IN (SELECT id FROM ne_comp_type WHERE name LIKE 'CBT OPS%' AND name NOT LIKE '%KPI SUM') AND name != '240-404-3136' AND name != '201-200-0101')
DATA_QRY_BLOCK_END

-- 
DATA_QRY_BLOCK_BEGIN DelDedicateds:
DELETE FROM dedicated WHERE collector_id IN 
( select id from collector where ne_id in 
		(select ne_id from ne_comp where ne_comp_type_id in 
			(select id from ne_comp_type where name like 'CBT OPS%' and name not like '%KPI SUM') and ne_id not like 3316 and ne_id not like 2957) 
			AND name like 'P%')
DATA_QRY_BLOCK_END

-- 
DATA_QRY_BLOCK_BEGIN DelStudyStatuses:
DELETE FROM study_status WHERE study_id IN 
( SELECT id FROM study WHERE collector_id IN 
	( select id from collector where ne_id in 
		(select ne_id from ne_comp where ne_comp_type_id in 
			(select id from ne_comp_type where name like 'CBT OPS%' and name not like '%KPI SUM') and ne_id not like 3316 and ne_id not like 2957) 
			AND name like 'P%') )
DATA_QRY_BLOCK_END

-- 
DATA_QRY_BLOCK_BEGIN DelStudies:
DELETE FROM study WHERE collector_id IN 
( select id from collector where ne_id in 
		(select ne_id from ne_comp where ne_comp_type_id in 
			(select id from ne_comp_type where name like 'CBT OPS%' and name not like '%KPI SUM') and ne_id not like 3316 and ne_id not like 2957) 
			AND name like 'P%')
DATA_QRY_BLOCK_END

-- 
DATA_QRY_BLOCK_BEGIN DelCollectors:
DELETE FROM collector WHERE ne_id in 
(select ne_id from ne_comp where ne_comp_type_id in 
			(select id from ne_comp_type where name like 'CBT OPS%' and name not like '%KPI SUM') and ne_id not like 3316 and ne_id not like 2957) AND name like 'P%'
DATA_QRY_BLOCK_END

-- 
DATA_QRY_BLOCK_BEGIN DelNeComps:
DELETE FROM ne_comp 
WHERE ne_comp_type_id in (select id from ne_comp_type where name like 'CBT OPS%' and name not like '%KPI SUM') and name != '240-404-3136' and name != '201-200-0101'
DATA_QRY_BLOCK_END