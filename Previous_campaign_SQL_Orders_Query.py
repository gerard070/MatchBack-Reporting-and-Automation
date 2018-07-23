import pandas as pd
import numpy as np

import sys
import os

from datetime import datetime, timedelta

sys.path.append(os.path.expanduser('~/Analytics/KenMyers/Utils'))
from SQL.Query import Query
from Email.SendMail import SendMail

import matplotlib.pyplot as plt
import seaborn as sns


from IPython.display import display, HTML

display(HTML(data="""
<style>
    div#notebook-container    { width: 95%; }
    div#menubar-container     { width: 65%; }
    div#maintoolbar-container { width: 99%; }
</style>
"""))

today_date = pd.to_datetime('today')
last_year = datetime(today_date.year-1, today_date.month, today_date.day-1)



#this is the query
q = """
select 
(CASE WHEN SUBSTR(MB_KEYCODE_LONG,3,4)< 4000 THEN 'House_List'
WHEN SUBSTR(MB_KEYCODE_LONG,3,4) Between 4000 AND 4999 THEN 'Optos'
WHEN SUBSTR(MB_KEYCODE_LONG,3,4) between 5000 and 7999 THEN 'Rentals'
WHEN SUBSTR(MB_KEYCODE_LONG,3,4) >= 8000 THEN 'Coops'
Else SUBSTR(MB_KEYCODE_LONG,3,4)
END) segment,
ORDER_ID ORDERS,
ORDER_TOTAL_SALE SALE_SKUS, ORDER_TOTAL_COST COST_SKUS,mb_program, hlink, 	to_char(ORDER_DATE, 'YYYY-MM-DD') as order_date 
FROM ORDERS
WHERE SUBSTR(MB_KEYCODE_LONG,1,2) IN ('7E')
and order_date <= TO_DATE('{}','yyyy-mm-dd')

""".format(last_year.strftime('%Y-%m-%d'))

# ugpostgres, marketing
data = Query('marketing',q)
data.to_csv("/opt/mnt/publicdrive/Analytics/Gerard/Matchback/MatchBack_8G_And_7E/Data/7E_Orders.csv", index=False)