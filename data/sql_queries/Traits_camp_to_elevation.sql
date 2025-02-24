-- Add a new column named 'elevation'
ALTER TABLE Traits
ADD COLUMN elevation INT;

-- Update the new column with the mapped values
UPDATE Traits
SET elevation = CASE camp
    WHEN 'MS' THEN 2250
    WHEN 'CL' THEN 1450
    WHEN 'PC' THEN 1100
    WHEN 'DG' THEN 650
END;

-- Verify the changes
SELECT camp, elevation FROM Traits;