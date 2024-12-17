#### U.S Household Income Dataset Cleaning ####

### Replace a column with '`ï»¿id' to id ########

ALTER TABLE us_household_income_statistics
RENAME COLUMN `ï»¿id` TO `id`;

### Next view the two table by joing them together #####

SELECT *
FROM
us_household_income uhi
JOIN 
us_household_income_statistics uhis
ON uhi.row_id = uhis.id;


###  Viewd the count of each table ######

SELECT COUNT(id)
FROM us_household_income;

SELECT COUNT(id)
FROM us_household_income_statistics;

####### Check for missing cell using the id column ####################
## Check for duplicate#################################

SELECT id, COUNT(id)
FROM us_household_income
GROUP BY id
HAVING COUNT(id)>1;

SELECT id, COUNT(id)
FROM us_household_income_statistics
GROUP BY id
HAVING COUNT(id)>1;

#########################################

SELECT *
FROM(
SELECT row_id, id,
ROW_NUMBER() OVER(PARTITION BY id ORDER BY id) row_num
FROM us_household_income
)duplicates
WHERE row_num >1;

#### Delect duplicate row ##########################

DELETE FROM us_household_income
WHERE row_id IN (
	SELECT row_id
	FROM(
SELECT row_id, id,
ROW_NUMBER() OVER(PARTITION BY id ORDER BY id) row_num
FROM us_household_income
)duplicates
WHERE row_num >1)
;

#### Next, checked lower case spelt name in the 'State' column #####

SELECT DISTINCT State_name
FROM us_household_income
ORDER BY 1
;

### Corrected the wrong State name. i.e State name that comes with lower case in the first alphabate###

UPDATE us_household_income
SET State_Name = 'Georgia' 
WHERE State_Name = 'georia'
;

UPDATE us_household_income
SET State_Name = 'Alabama' 
WHERE State_Name = 'Alabama'
;

######## Checked and fixed the 'State' column abbrivation  #######

SELECT *
FROM us_household_income
WHERE County ='Autauga County'
ORDER BY 1;

UPDATE us_household_income
SET Place = 'Autaugaville'
WHERE County ='Autauga County'
AND City ='Vinemont'
;


########## Next i checked the 'Type' column #################

SELECT Type, COUNT(Type)
FROM us_household_income
GROUP BY Type
ORDER BY 1
;

UPDATE us_household_income
SET Type = 'Borough'
WHERE Type ='Boroughs'
;

##################################### Checked the ALand, AWater column with water and land #####
SELECT ALand, AWater
FROM  us_household_income
WHERE  (ALand =0 OR ALand ='' OR AWater IS NULL);


######### Next Section ~~~ Doing more EDA 
##### Looking for pattern and trend in the dataset  ###############


#### Exploring which area in the U.S has more Land and which area has more Water for each State #####

SELECT State_Name, SUM(ALand), SUM(AWater)
FROM us_household_income
GROUP BY State_Name
ORDER BY 3 DESC
LIMIT 10;


################ Joining the income dataset and the statistic dataset together ########

SELECT * 
FROM us_household_income ui
JOIN us_household_income_statistics us
ON ui.id=us.id;

SELECT * 
FROM us_household_income ui
RIGHT JOIN us_household_income_statistics us
ON ui.id=us.id
WHERE ui.id IS NULL;


SELECT * 
FROM us_household_income ui
INNER JOIN us_household_income_statistics us
ON ui.id=us.id
;



SELECT * 
FROM us_household_income ui
INNER JOIN us_household_income_statistics us
ON ui.id=us.id
WHERE Mean<> 0
;

##### Explore State_name with the County ###

SELECT ui.State_Name, County, 
Type, `Primary`, Mean, Median
FROM us_household_income ui
INNER JOIN us_household_income_statistics us
ON ui.id=us.id
WHERE Mean<> 0
;


SELECT ui.State_Name, AVG(Mean), AVG(Median)
FROM us_household_income ui
INNER JOIN us_household_income_statistics us
ON ui.id=us.id
WHERE Mean<> 0
GROUP BY ui.State_Name
;

SELECT ui.State_Name,ROUND( AVG(Mean),1), ROUND(AVG(Median),1)
FROM us_household_income ui
INNER JOIN us_household_income_statistics us
ON ui.id=us.id
WHERE Mean<> 0
GROUP BY ui.State_Name
ORDER BY 2 DESC
LIMIT 10
;


SELECT ui.State_Name,ROUND( AVG(Mean),1), ROUND(AVG(Median),1)
FROM us_household_income ui
INNER JOIN us_household_income_statistics us
ON ui.id=us.id
WHERE Mean<> 0
GROUP BY ui.State_Name
ORDER BY 3 DESC
LIMIT 10
;


SELECT Type,ROUND(AVG(Mean),1), ROUND(AVG(Median),1)
FROM us_household_income ui
INNER JOIN us_household_income_statistics us
ON ui.id=us.id
WHERE Mean<> 0
GROUP BY 1
ORDER BY 3 DESC
LIMIT 20
;

SELECT Type,COUNT(Type), ROUND(AVG(Mean),1),
ROUND(AVG(Median),1)
FROM us_household_income ui
INNER JOIN us_household_income_statistics us
ON ui.id=us.id
WHERE Mean<> 0
GROUP BY 1
ORDER BY 3 DESC
LIMIT 20
;


SELECT Type,COUNT(Type), ROUND(AVG(Mean),1),
ROUND(AVG(Median),1)
FROM us_household_income ui
INNER JOIN us_household_income_statistics us
ON ui.id=us.id
WHERE Mean<> 0
GROUP BY 1
ORDER BY 4 DESC
LIMIT 20
;


SELECT Type,COUNT(Type), ROUND(AVG(Mean),1),
ROUND(AVG(Median),1)
FROM us_household_income ui
INNER JOIN us_household_income_statistics us
ON ui.id=us.id
WHERE Mean<> 0
GROUP BY 1
HAVING COUNT(Type)> 100
ORDER BY 4 DESC
LIMIT 20
;


SELECT * FROM us_household_income
WHERE Type ='Community'
;

######### Remove outliers if needed #############

SELECT ui.State_Name, City, ROUND(AVG(Mean),1), ROUND(AVG(Median),1)
FROM us_household_income ui
JOIN us_household_income_statistics us
ON ui.id =us.id
GROUP BY ui.State_Name, City
ORDER BY ROUND(AVG(Mean),1) DESC;


