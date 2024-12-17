### Data cleaning for bakery.customer_sweepstakes

##      ORDER USED IN CLEANING THIS DATASET

# 1.	Remove duplicate
# 2.	Standardize data
# 3.	Breaking One column into multiple columns
# 4.	Working with NULL
# 5.	Delecting columns
# 6.	ALERT TABLE 
# 7.	DROP 
# 8.	UPDATE TABLE
# 9.	SET COLO=COMMAND


#### Section 1 ##############################################

## Import file and check for dupilcate   #####

SELECT * FROM bakery.customer_sweeptt;

ALTER TABLE bakery.customer_sweeptt
RENAME COLUMN `ï»¿sweepstake_id` TO `sweept_id`;

### Identified the duplicates_________________________~~~~~~~~~#######

SELECT customer_id, COUNT(customer_id)
 FROM bakery.customer_sweeptt
 GROUP BY customer_id
 HAVING COUNT(customer_id) >1;
 
 ### OR we can use a windows function to check duplicate ##########
 
 SELECT*
 FROM
 (SELECT customer_id,
 ROW_NUMBER() OVER(PARTITION BY customer_id ORDER BY customer_id) AS row_num
 FROM bakery.customer_sweeptt) AS table_row
 WHERE row_num > 1
 ;
 
 
 ### Delete duplicates ~~~~~~~~~~~~~~~~~~~#################
 
DELETE FROM bakery.customer_sweeptt
WHERE sweept_id IN (
    SELECT sweept_id
    FROM (
        SELECT sweept_id,
		ROW_NUMBER() OVER (PARTITION BY customer_id ORDER BY sweept_id) AS row_num
        FROM bakery.customer_sweeptt) 
        AS temp
    WHERE row_num > 1
);
 
 ### Check the dataset   ###################
 
SELECT * FROM bakery.customer_sweeptt
 ;
 
 
 ##### Next is to Standarizing the dataset in the column ###################
 
 SELECT phone, REGEXP_REPLACE(phone, '[()-/+]', '')
 FROM customer_sweeptt;
 
 ###### UPDATE ######
 
 UPDATE customer_sweeptt
 SET phone = REGEXP_REPLACE(phone, '[()-/+]', '');
 
 
 ### Check update ______________________~~~~####################
 
 SELECT * FROM bakery.customer_sweeptt
 ;
 
 #########################  Making the phone number callable #######################################
 
  SELECT phone, CONCAT
  (SUBSTRING(phone,1,3),'-', SUBSTRING(phone, 4,3),
  '-',SUBSTRING(phone, 7,4))
 FROM customer_sweeptt
 WHERE phone <> '';
 
 ###### UPDATE ######
 
 
 UPDATE customer_sweeptt
 SET phone =  CONCAT
  (SUBSTRING(phone,1,3),'-', SUBSTRING(phone, 4,3),
  '-',SUBSTRING(phone, 7,4))
 WHERE phone <> '';
 

 
###  Next working with the birth_date column____________

SELECT birth_date,
 CONCAT(SUBSTRING(birth_date,9,2),'/',
 SUBSTRING(birth_date,6,2),'/',
SUBSTRING(birth_date,1,4))
FROM customer_sweeptt;

######          UPDATE         ########################

UPDATE customer_sweeptt
SET birth_date= CONCAT(SUBSTRING(birth_date,9,2),'/',
 SUBSTRING(birth_date,6,2),'/',
SUBSTRING(birth_date,1,4))
WHERE sweept_id IN (9,11);

UPDATE customer_sweeptt
SET birth_date = STR_TO_DATE(birth_date, '%m/%d/%Y');


############################# Using CASE stament____________####################

SELECT `Are you over 18?`,
CASE
WHEN `Are you over 18?` ='Yes' THEN 'Y'
WHEN `Are you over 18?` = 'NO' THEN 'N'
ELSE `Are you over 18?`
END
FROM customer_sweeptt
;
 
 
 UPDATE customer_sweeptt
 SET
 `Are you over 18?` = 
 CASE
WHEN `Are you over 18?` ='Yes' THEN 'Y'
WHEN `Are you over 18?` = 'NO' THEN 'N'
ELSE `Are you over 18?`
END;


#### Breaking one column into Multiple columns     #######


 
 SELECT address, SUBSTRING_INDEX
 (address,',',1),
 SUBSTRING_INDEX(address,',',-1)
 FROM customer_sweeptt
 ;
 
SELECT address,
SUBSTRING_INDEX(address,',',1) AS Street, 
SUBSTRING_INDEX(SUBSTRING_INDEX
(address,',',2),',',-1) AS City,
SUBSTRING_INDEX(address,',',-1) AS State
FROM customer_sweeptt
 ;

#### Create a table to update the address in the dataset


ALTER TABLE customer_sweeptt
 ADD COLUMN city VARCHAR(50) AFTER street,
 ADD COLUMN state VARCHAR(50) AFTER city
 ;

UPDATE customer_sweeptt
SET street = SUBSTRING_INDEX(address,',',1);

UPDATE customer_sweeptt
SET city = SUBSTRING_INDEX(SUBSTRING_INDEX
(address,',',2),',',-1);

UPDATE customer_sweeptt
SET state = SUBSTRING_INDEX(address,',',-1);


### Next capitalize the State #######################

SELECT state, UPPER(state)
FROM customer_sweeptt;
 
UPDATE customer_sweeptt
SET state = UPPER(state);


####### Next i removed spacing #####################

SELECT city, TRIM(city)
FROM customer_sweeptt;
 
 UPDATE customer_sweeptt
 SET city = TRIM(city);
 
SELECT state, TRIM(state)
FROM customer_sweeptt;
 
 UPDATE customer_sweeptt
 SET state = TRIM(state);

 
#### Working with null values ~~~~~~~~~~~~~~~~~##############

SELECT COUNT(sweept_id),COUNT(phone)
FROM customer_sweeptt;

UPDATE customer_sweeptt
SET phone = NULL
WHERE phone =''; 

UPDATE customer_sweeptt
SET income = NULL
WHERE income =''; 

SELECT AVG(COALESCE(income,0))
FROM customer_sweeptt
;
SELECT birth_date, `Are you over 18?`
FROM customer_sweeptt
;

UPDATE customer_sweeptt
SET `Are you over 18?` = 'N'
WHERE (YEAR(NOW())-18) < YEAR(birth_date)
;

UPDATE customer_sweeptt
SET `Are you over 18?` = 'Y'
WHERE (YEAR(NOW())-18) > YEAR(birth_date)
;

### Delecting Columns #####

ALTER TABLE customer_sweeptt
DROP COLUMN address;

ALTER TABLE customer_sweeptt
DROP COLUMN favorite_color;



##############  END__________ ################
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 


