SELECT 
    sp_code, 
    season, 
    elevation,
	species,
	plant_group,
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
	concentration,
	sugar_amount,
	glucose,
	fructose,
	saccharose
FROM 
    combined
GROUP BY 
    sp_code, 
    season, 
    elevation;