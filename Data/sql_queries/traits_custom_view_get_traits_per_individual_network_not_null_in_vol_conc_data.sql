SELECT 
    sp_code, 
    season, 
    elevation,
	species,
	shape_upd,
	symmetry,
	flower_pos,
	anther_pos,
	odour,
	brightness,
	colour,
	nectar_guides,
	size,
	tube_length,
	volume,
	concentration--,
-- 	sugar_amount,
-- 	glucose,
-- 	fructose,
-- 	saccharose
FROM 
    traits_unique_per_network_not_null
WHERE
	volume IS NOT NULL AND
	concentration IS NOT NULL --AND
-- -- 	sugar_amount IS NOT NULL AND
-- -- 	glucose IS NOT NULL AND
-- -- 	fructose IS NOT NULL AND
-- -- 	saccharose IS NOT NULL
GROUP BY
	sp_code, season, elevation