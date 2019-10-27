# This .py file holds functions that will keep our code a little bit
# easier to read.
from google.cloud import bigquery
from google.oauth2 import service_account


def tbl_exists(client, table_ref):
    from google.cloud.exceptions import NotFound
    
    try:
        client.get_table(table_ref)
        return True
    except NotFound:
        return False
    
def clean_out_gbq_tables(client,proj_id,ds_id) :
    # Cleans out all the tables in the data set
    dataset = client.get_dataset(".".join([proj_id,ds_id]))
    tables = list(client.list_tables(dataset))
    
    num_deleted = 0
    
    for table in tables :
        t_id = table.table_id
        client.delete_table(".".join([proj_id,ds_id,t_id]), not_found_ok=True)
        num_deleted += 1
        
    print("Just deleted {} tables from {}".format(num_deleted,".".join([proj_id,ds_id])))
    
    return(num_deleted)
    
    
def get_upload_job_config() :
    # Builds the job-upload configuration. 
    
    job_config = bigquery.LoadJobConfig()
    job_config.write_disposition = bigquery.WriteDisposition.WRITE_APPEND
    job_config.schema_update_options = [
        bigquery.SchemaUpdateOption.ALLOW_FIELD_ADDITION # This allows us to modify the table. 
    ]
    
    job_config.schema = [
        bigquery.SchemaField("datetime", "TIMESTAMP", mode="NULLABLE"),
        bigquery.SchemaField("register_no", "FLOAT", mode="NULLABLE"),
        bigquery.SchemaField("emp_no", "FLOAT", mode="NULLABLE"),
        bigquery.SchemaField("trans_no", "FLOAT", mode="NULLABLE"),
        bigquery.SchemaField("upc", "STRING", mode="NULLABLE"),
        bigquery.SchemaField("description", "STRING", mode="NULLABLE"),
        bigquery.SchemaField("trans_type", "STRING", mode="NULLABLE"),
        bigquery.SchemaField("trans_subtype", "STRING", mode="NULLABLE"),
        bigquery.SchemaField("trans_status", "STRING", mode="NULLABLE"),
        bigquery.SchemaField("department", "FLOAT", mode="NULLABLE"),
        bigquery.SchemaField("quantity", "FLOAT", mode="NULLABLE"),
        bigquery.SchemaField("Scale", "FLOAT", mode="NULLABLE"),
        bigquery.SchemaField("cost", "FLOAT", mode="NULLABLE"),
        bigquery.SchemaField("unitPrice", "FLOAT", mode="NULLABLE"),
        bigquery.SchemaField("total", "FLOAT", mode="NULLABLE"),
        bigquery.SchemaField("regPrice", "FLOAT", mode="NULLABLE"),
        bigquery.SchemaField("altPrice", "FLOAT", mode="NULLABLE"),
        bigquery.SchemaField("tax", "FLOAT", mode="NULLABLE"),
        bigquery.SchemaField("taxexempt", "FLOAT", mode="NULLABLE"),
        bigquery.SchemaField("foodstamp", "FLOAT", mode="NULLABLE"),
        bigquery.SchemaField("wicable", "FLOAT", mode="NULLABLE"),
        bigquery.SchemaField("discount", "FLOAT", mode="NULLABLE"),
        bigquery.SchemaField("memDiscount", "FLOAT", mode="NULLABLE"),
        bigquery.SchemaField("discountable", "FLOAT", mode="NULLABLE"),
        bigquery.SchemaField("discounttype", "FLOAT", mode="NULLABLE"),
        bigquery.SchemaField("voided", "FLOAT", mode="NULLABLE"),
        bigquery.SchemaField("percentDiscount", "FLOAT", mode="NULLABLE"),
        bigquery.SchemaField("ItemQtty", "FLOAT", mode="NULLABLE"),
        bigquery.SchemaField("volDiscType", "FLOAT", mode="NULLABLE"),
        bigquery.SchemaField("volume", "FLOAT", mode="NULLABLE"),
        bigquery.SchemaField("VolSpecial", "FLOAT", mode="NULLABLE"),
        bigquery.SchemaField("mixMatch", "FLOAT", mode="NULLABLE"),
        bigquery.SchemaField("matched", "FLOAT", mode="NULLABLE"),
        bigquery.SchemaField("memType", "BOOLEAN", mode="NULLABLE"),
        bigquery.SchemaField("staff", "BOOLEAN", mode="NULLABLE"),
        bigquery.SchemaField("numflag", "FLOAT", mode="NULLABLE"),
        bigquery.SchemaField("itemstatus", "FLOAT", mode="NULLABLE"),
        bigquery.SchemaField("tenderstatus", "FLOAT", mode="NULLABLE"),
        bigquery.SchemaField("charflag", "STRING", mode="NULLABLE"),
        bigquery.SchemaField("varflag", "FLOAT", mode="NULLABLE"),
        bigquery.SchemaField("batchHeaderID", "BOOLEAN", mode="NULLABLE"),
        bigquery.SchemaField("local", "FLOAT", mode="NULLABLE"),
        bigquery.SchemaField("organic", "FLOAT", mode="NULLABLE"),
        bigquery.SchemaField("display", "BOOLEAN", mode="NULLABLE"),
        bigquery.SchemaField("receipt", "FLOAT", mode="NULLABLE"),
        bigquery.SchemaField("card_no", "FLOAT", mode="NULLABLE"),
        bigquery.SchemaField("store", "FLOAT", mode="NULLABLE"),
        bigquery.SchemaField("branch", "FLOAT", mode="NULLABLE"),
        bigquery.SchemaField("match_id", "FLOAT", mode="NULLABLE"),
        bigquery.SchemaField("trans_id", "FLOAT", mode="NULLABLE"),
    ]
    job_config.source_format = bigquery.SourceFormat.CSV

    return(job_config)

def create_table(cursor, table_name) :
    # Create our tables. table_name is 
    # just a string that tells us which one to make. 
    
    if table_name == "date_hour" :
        cursor.execute('''DROP TABLE IF EXISTS date_hour''')
        cursor.execute('''CREATE TABLE date_hour (
                               date TEXT,
                               hour INTEGER,
                               sales REAL,
                               transactions INTEGER,
                               items INTEGER)
                    ''')
    elif table_name == "owner_year_month" :
        cursor.execute('''DROP TABLE IF EXISTS owner_year_month''')
        cursor.execute('''CREATE TABLE owner_year_month (
                               card_no INTEGER,
                               year INTEGER,
                               month INTEGER, 
                               sales REAL,
                               transactions INTEGER,
                               items INTEGER)
                    ''')
    elif table_name == "product_year_month" :
        cursor.execute('''DROP TABLE IF EXISTS product_year_month''')
        cursor.execute('''CREATE TABLE product_year_month (
                               upc TEXT,
                               description TEXT,
                               dept_num INTEGER,
                               dept_name TEXT,
                               year INTEGER,
                               month INTEGER, 
                               sales REAL,
                               transactions INTEGER,
                               items INTEGER)
                    ''')
    else :
        print("Didn't recognize the table id '{}'. Ignoring.".format(table_name))
        
    return(0)
    
    
def get_gbq_query(table_name) :
    # A centralized function to hold the queries
    # for GBQ. 
    if table_name == "date_hour" :
        # Did this glue-strings-together thing here, but 
        # it's a pain. Switching back to triple quotes for others.
        query = (
            'SELECT EXTRACT(DATE from datetime) as date, '
            'EXTRACT(Hour from datetime) as hour, '
            'ROUND(SUM(total),2) AS sales, '
            'COUNT(distinct( '
            'CONCAT(CAST(EXTRACT(DATE from datetime) AS STRING), '
                    'CAST(register_no AS STRING), '
                    'CAST(emp_no AS STRING), '
                    'CAST(trans_no AS STRING)))) as transactions, '
             'SUM(CASE WHEN (trans_status = "V" or trans_status = "R") '
                    'THEN -1 ELSE 1 END) as items '
             'FROM `umt-msba.wedge_transactions.transArchive_*` '
             'WHERE department != 0 and '
                  'department != 15 and '
                  'trans_status != "M" and '
                  'trans_status != "C" and '
                  'trans_status != "J" and '
                 '(trans_status IS NULL or  '
                  'trans_status = " " or ' 
                  'trans_status = "V" or ' 
                  'trans_status = "R") '
             'GROUP BY date, hour ' 
             'ORDER BY date, hour'        
        )
    elif table_name == "owner_year_month" :
        query = """
                   SELECT card_no,
                   EXTRACT(YEAR from datetime) as year,
                   EXTRACT(MONTH from datetime) as month,
                   ROUND(SUM(total),2) AS sales,
                   COUNT(distinct(
                      CONCAT(CAST(EXTRACT(DATE from datetime) AS STRING),
                             CAST(register_no AS STRING),
                             CAST(emp_no AS STRING),
                             CAST(trans_no AS STRING)))) as transactions,
                   SUM(CASE WHEN (trans_status = 'V' or trans_status = 'R') THEN -1 ELSE 1 END) as items
                   FROM `umt-msba.wedge_transactions.transArchive_*`
                     WHERE department != 0 and
                          department != 15 and
                          trans_status != 'M' and
                          trans_status != 'C' and
                          trans_status != 'J' and
                         (trans_status IS NULL or 
                          trans_status = ' ' or 
                          trans_status = 'V' or 
                          trans_status = 'R') 
                    GROUP BY card_no, year, month
                    ORDER BY card_no, year, month        
        """
    elif table_name == "product_year_month" :
        query = """
                SELECT
                  upc,
                  LOWER(description) AS description,
                  tr.department AS dept_num,
                  lu.dept_name,
                  EXTRACT(YEAR
                  FROM
                    datetime) AS year,
                  EXTRACT(MONTH
                  FROM
                    datetime) AS month,
                  ROUND(SUM(total),2) AS sales,
                  COUNT(DISTINCT( CONCAT(CAST(EXTRACT(DATE
                          FROM
                            datetime) AS STRING), 
                            CAST(register_no AS STRING), 
                            CAST(emp_no AS STRING), 
                            CAST(trans_no AS STRING)))) AS transactions,
                  SUM(CASE
                      WHEN (trans_status = 'V' OR trans_status = 'R') THEN -1
                    ELSE
                    1
                  END
                    ) AS items
                FROM
                  `umt-msba.wedge_transactions.transArchive_*` AS tr
                LEFT OUTER JOIN
                  `umt-msba.wedge_transactions.department_lookup` AS lu
                ON
                  lu.department = tr.department
                WHERE
                  tr.department != 0
                  AND tr.department != 15
                  AND trans_status != 'M'
                  AND trans_status != 'C'
                  AND trans_status != 'J'
                  AND (trans_status IS NULL
                    OR trans_status = ' '
                    OR trans_status = 'V'
                    OR trans_status = 'R')
                GROUP BY
                  upc,
                  description,
                  dept_num,
                  dept_name,
                  year,
                  month
                ORDER BY
                  description,
                  year,
                  month
        """
    else :
        print("Didn't recognize the table id '{}'. Ignoring.".format(table_id))
        
    return(query)


def fill_db_table(cursor, table_name, qry) :
    # Assumes we have a table shaped appropriately for our query results. 
    uploaded_rows = 0
    
    if table_name == "date_hour" :
        for row in qry :
            cursor.execute("""INSERT INTO date_hour 
                              (date, hour, sales, transactions, items)
                              VALUES (?,?,?,?,?)""",row)
            uploaded_rows += 1            
    elif table_name == "owner_year_month" :
        for row in qry :
            cursor.execute("""INSERT INTO owner_year_month 
                              (card_no, year, month, sales, transactions, items)
                              VALUES (?,?,?,?,?,?)""",row)
            uploaded_rows += 1            
    elif table_name == "product_year_month" :
        for row in qry :
            cursor.execute("""INSERT INTO product_year_month 
                              (upc, description, dept_name, dept_num,
                              year, month, sales, transactions, items)
                              VALUES (?,?,?,?,?,?,?,?,?)""",row)
            uploaded_rows += 1            
    else :
        print("Didn't recognize the table id '{}'. Ignoring.".format(table_name))
        
    print("Loaded {} rows into {}.".format(uploaded_rows,table_name))
    
    return(uploaded_rows)
        
