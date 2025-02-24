UPDATE combined
SET 
    pollen_feed = COALESCE(NULLIF(pollen_feed, ' '), 0),
    dur_pollen_feed = COALESCE(NULLIF(dur_pollen_feed, ' '), 0),
    nectar_feed = COALESCE(NULLIF(nectar_feed, ' '), 0),
    dur_nectar_feed = COALESCE(NULLIF(dur_nectar_feed, ' '), 0),
    unsuc_feed = COALESCE(NULLIF(unsuc_feed, ' '), 0),
    dur_unsuc_feed = COALESCE(NULLIF(dur_unsuc_feed, ' '), 0),
    floral_parts_feed = COALESCE(NULLIF(floral_parts_feed, ' '), 0),
    dur_floral_parts_feed = COALESCE(NULLIF(dur_floral_parts_feed, ' '), 0),
    smelling = COALESCE(NULLIF(smelling, ' '), 0),
    dur_smelling = COALESCE(NULLIF(dur_smelling, ' '), 0),
    anthers_contact = COALESCE(NULLIF(anthers_contact, ' '), 0),
    no_anthers_contact = COALESCE(NULLIF(no_anthers_contact, ' '), 0),
    stigmas_contact = COALESCE(NULLIF(stigmas_contact, ' '), 0),
    no_stigmas_contact = COALESCE(NULLIF(no_stigmas_contact, ' '), 0),
    no_robbed = COALESCE(NULLIF(no_robbed, ' '), 0),
    robbing = COALESCE(NULLIF(robbing, ' '), 0);
