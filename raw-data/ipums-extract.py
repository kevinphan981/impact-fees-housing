from zipfile import ZipFile
import pandas as pd

path = "raw-data"
names = ZipFile(path).namelist()

with ZipFile(path) as z:
    with z.open(names[0]) as f:
        data = pd.read_csv(f)