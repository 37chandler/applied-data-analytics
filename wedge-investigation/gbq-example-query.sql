SELECT
  card_no,
  department,
  EXTRACT(Year
  FROM
    datetime) AS year,
  EXTRACT(Month
  FROM
    datetime) AS month,
  SUM(total) AS spend,
  COUNT(DISTINCT(CONCAT(CAST(DATE(datetime) AS string), 
                        " ", 
                        CAST(register_no AS string), 
                        " ", 
                        CAST(emp_no AS string), 
                        " ",
                        CAST(trans_no AS string)))) AS Transactions,
  SUM(CASE
      WHEN (trans_status = 'V' OR trans_status = 'R') THEN -1
    ELSE
    1
  END
    ) AS Items
FROM
  `umt-msba.wedge_example.transactions_201307_small`
WHERE
  department != 0
  AND department != 15
  AND trans_status != 'M'
  AND trans_status != 'C'
  AND trans_status != 'J'
  AND (trans_status = ''
    OR trans_status = ' '
    OR trans_status = 'V'
    OR trans_status = 'R')
GROUP BY
  card_no,
  department,
  year,
  month