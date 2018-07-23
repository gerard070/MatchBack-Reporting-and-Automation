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


#this is the query

q = """
Select segment, List_Number, DESCRIP, CURRENT_HLINK, ORIG_HLINK, MailDate,

(CASE WHEN segment = 'House_List' THEN SUBSTR(KEYCODE,10,1)
ELSE SUBSTR(KEYCODE,3,4)
END) AS List

From
(select 
(CASE WHEN SUBSTR(KEYCODE,3,4)< 4000 THEN 'House_List'
WHEN SUBSTR(KEYCODE,3,4) Between 4000 AND 4999 THEN 'Optos'
WHEN SUBSTR(KEYCODE,3,4) between 5000 and 7999 THEN 'Rentals'
WHEN SUBSTR(KEYCODE,3,4) >= 8000 THEN 'Coops'
Else SUBSTR(KEYCODE,3,4)
END) segment, SUBSTR(KEYCODE,3,4) as List_Number,
LIST_DESC DESCRIP,CURRENT_HLINK, ORIG_HLINK, MailDate, KEYCODE
 FROM MAIL_PROMOS 
 WHERE SUBSTR(KEYCODE,1,2) IN ('8G')
) data

"""


# ugpostgres, marketing
data = Query('marketing',q)






data.to_csv("/opt/mnt/publicdrive/Analytics/Gerard/Matchback/MatchBack_8G_And_7E/Data/8G_Mail_Promos.csv", index=False)