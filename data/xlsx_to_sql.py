import pandas as pd
import sqlite3

excel_path = "Data Kobe export.xlsx"

# Read Excel Sheets
df_combined = pd.read_excel(excel_path, sheet_name='Combined')
#df_combined_pivot = pd.read_excel(excel_path, sheet_name='Combined Pivot')
df_visitors = pd.read_excel(excel_path, sheet_name='Visitors')
#df_visitors_pivot = pd.read_excel(excel_path, sheet_name='Pivot Visitors')
df_nectar = pd.read_excel(excel_path, sheet_name='Nectar')
#df_nectar_pivot = pd.read_excel(excel_path, sheet_name='Pivot Nectar')
df_traits = pd.read_excel(excel_path, sheet_name='TRAITSFull')
#df_traits_pivot = pd.read_excel(excel_path, sheet_name='Pivot traits')

# Create SQLite Database
conn = sqlite3.connect("Dataset.db")

# Write DataFrames into SQLite
df_combined.to_sql("Combined", conn, if_exists='replace', index=False)
#df_combined_pivot.to_sql("Combined Pivot", conn, if_exists='replace', index=False)
df_visitors.to_sql("Visitors", conn, if_exists='replace', index=False)
#df_visitors_pivot.to_sql("Visitors Pivot", conn, if_exists='replace', index=False)
df_nectar.to_sql("Nectar", conn, if_exists='replace', index=False)
#df_nectar_pivot.to_sql("Nectar Pivot", conn, if_exists='replace', index=False)
df_traits.to_sql("Traits", conn, if_exists='replace', index=False)
#df_traits_pivot.to_sql("Traits Pivot", conn, if_exists='replace', index=False)