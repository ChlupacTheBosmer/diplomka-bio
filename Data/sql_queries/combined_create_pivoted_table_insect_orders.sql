-- Can be used for creating a new TABLE of visits per plant species grouping by insect order. This aims to mimic Yannicks pivot table and can be used for easily reconstructing the pollination network using bipartite.
-- Note that species column is included to provide more informative labels in plots etc. However, some species do not have the species filled in and in that case "unknown" is used to prevent NULL values.
-- Note that you can either filter based on the "include_in_network" column from the "combined" table or not. Just comment out the WHERE statement below.

CREATE TABLE visits_pivot_insect_order_include AS
SELECT
    sp_code,
    season,
    elevation,
	COALESCE(species, 'unknown') AS species,
    SUM(CASE WHEN insect_order = '*Unknown*' THEN freq_fm_spp ELSE 0 END) AS unknown,
    SUM(CASE WHEN insect_order = 'Acariformes' THEN freq_fm_spp ELSE 0 END) AS acariformes,
    SUM(CASE WHEN insect_order = 'Annelida' THEN freq_fm_spp ELSE 0 END) AS annelida,
    SUM(CASE WHEN insect_order = 'Araneae' THEN freq_fm_spp ELSE 0 END) AS araneae,
    SUM(CASE WHEN insect_order = 'Archaeognatha' THEN freq_fm_spp ELSE 0 END) AS archaeognatha,
    SUM(CASE WHEN insect_order = 'Blattodea' THEN freq_fm_spp ELSE 0 END) AS blattodea,
    SUM(CASE WHEN insect_order = 'Chilopoda' THEN freq_fm_spp ELSE 0 END) AS chilopoda,
    SUM(CASE WHEN insect_order = 'Chiroptera' THEN freq_fm_spp ELSE 0 END) AS chiroptera,
    SUM(CASE WHEN insect_order = 'Coleoptera' THEN freq_fm_spp ELSE 0 END) AS coleoptera,
    SUM(CASE WHEN insect_order = 'Collembola' THEN freq_fm_spp ELSE 0 END) AS collembola,
    SUM(CASE WHEN insect_order = 'Dermaptera' THEN freq_fm_spp ELSE 0 END) AS dermaptera,
    SUM(CASE WHEN insect_order = 'Diptera' THEN freq_fm_spp ELSE 0 END) AS diptera,
    SUM(CASE WHEN insect_order = 'Gastropoda' THEN freq_fm_spp ELSE 0 END) AS gastropoda,
    SUM(CASE WHEN insect_order = 'Haplotaxida' THEN freq_fm_spp ELSE 0 END) AS haplotaxida,
    SUM(CASE WHEN insect_order = 'Hemiptera' THEN freq_fm_spp ELSE 0 END) AS hemiptera,
    SUM(CASE WHEN insect_order = 'Hirudinea' THEN freq_fm_spp ELSE 0 END) AS hirudinea,
    SUM(CASE WHEN insect_order = 'Hymenoptera' THEN freq_fm_spp ELSE 0 END) AS hymenoptera,
    SUM(CASE WHEN insect_order = 'Isopoda' THEN freq_fm_spp ELSE 0 END) AS isopoda,
    SUM(CASE WHEN insect_order = 'Lepidoptera' THEN freq_fm_spp ELSE 0 END) AS lepidoptera,
    SUM(CASE WHEN insect_order = 'Myriapoda' THEN freq_fm_spp ELSE 0 END) AS myriapoda,
    SUM(CASE WHEN insect_order = 'Neuroptera' THEN freq_fm_spp ELSE 0 END) AS neuroptera,
    SUM(CASE WHEN insect_order = 'Opiliones' THEN freq_fm_spp ELSE 0 END) AS opiliones,
    SUM(CASE WHEN insect_order = 'Orthoptera' THEN freq_fm_spp ELSE 0 END) AS orthoptera,
    SUM(CASE WHEN insect_order = 'Passeriformes' THEN freq_fm_spp ELSE 0 END) AS passeriformes,
    SUM(CASE WHEN insect_order = 'Phasmatodea' THEN freq_fm_spp ELSE 0 END) AS phasmatodea,
    SUM(CASE WHEN insect_order = 'Phthiraptera' THEN freq_fm_spp ELSE 0 END) AS phthiraptera,
    SUM(CASE WHEN insect_order = 'Psocoptera' THEN freq_fm_spp ELSE 0 END) AS psocoptera,
    SUM(CASE WHEN insect_order = 'Scincidae' THEN freq_fm_spp ELSE 0 END) AS scincidae,
    SUM(CASE WHEN insect_order = 'Small_mammals' THEN freq_fm_spp ELSE 0 END) AS small_mammals,
    SUM(CASE WHEN insect_order = 'Thysanoptera' THEN freq_fm_spp ELSE 0 END) AS thysanoptera,
    SUM(CASE WHEN insect_order = 'Trichoptera' THEN freq_fm_spp ELSE 0 END) AS trichoptera,
    SUM(freq_fm_spp) AS total_visits
FROM combined
WHERE
	include_in_network == 1
GROUP BY sp_code, season, elevation;