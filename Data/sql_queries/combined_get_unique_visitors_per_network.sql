SELECT visitor, elevation, season, insect_order, functional_group
FROM
	combined
WHERE
	include_in_network == 1 AND elevation > 600
GROUP BY
	visitor, elevation, season
