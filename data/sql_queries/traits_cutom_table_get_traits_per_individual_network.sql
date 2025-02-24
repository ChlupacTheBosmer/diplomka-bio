-- This will create a new table with individual entries for each plant species in every network (as unique combos of sp_code, elevation, season) with the trait measurements.
-- Note that also entries that do not have the trait data filled in will be included. These should filtered out upon querying from the database. You can filter out those "WHERE species IS NOT NULL".
-- Furthermore, you may wish to filter out those that do not have NULL in volume and/or sugar_amount depending on whether you do want to include the nectar data in your analysis.

CREATE TABLE traits_unique_per_network AS
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