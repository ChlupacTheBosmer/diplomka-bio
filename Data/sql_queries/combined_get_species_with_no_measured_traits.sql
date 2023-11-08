SELECT sp_code, MIN(hdd_code) as hdd_code, MIN(plant_group) as plant_group, MIN(elevation) as elevation, MIN(season) as season
FROM combined
WHERE species IS NULL
GROUP BY sp_code;