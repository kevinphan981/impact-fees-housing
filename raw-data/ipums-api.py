import os
from pathlib import Path
from ipumspy import IpumsApiClient, MicrodataExtract, AggregateDataExtract, NhgisDataset, readers, ddi, NhgisDataTableMetadata
from dotenv import load_dotenv

# reads .env and sets environment variables
load_dotenv()  

# load in api key
IPUMS_API_KEY = os.environ.get("ipums_key")
ipums = IpumsApiClient(IPUMS_API_KEY)

# for page in ipums.get_metadata_catalog("nhgis", metadata_type="data_tables"):
#    print(page)
#    # will have to add something else to filter for specifically housing related stuff.


# list of tables to fetch
data_tables = [""]


# meta data reading to determine tables to select
test = NhgisDataTableMetadata("B25003H", "2018_2022_ACS5a")
print(test.nhgis_code) #keeps returning none...

# # create an extract definition
# extract = AggregateDataExtract(
#    collection="nhgis",
#    description="Extent selection example",
#    datasets=[
#       NhgisDataset(name="2018_2022_ACS5a", data_tables=["B01001"], geog_levels=["blck_grp"]),
#       NhgisDataset(name="2017_2021_ACS5a", data_tables=["B01001"], geog_levels=["blck_grp"])
#    ],
#    geographic_extents=["010", "050"]
# )

# # Submit the extract request
# ipums.submit_extract(extract)
# print(f"Extract submitted with id {extract.extract_id}")
# #> Extract submitted with id 1

# # check extract status
# extract_status = ipums.extract_status(extract=1, collection="usa") #collection may be diff
# print(f"extract is {extract_status}")


# # Wait for the extract to finish
# ipums.wait_for_extract(extract)

# # Download the extract
# DOWNLOAD_DIR = Path("raw-data")
# ipums.download_extract(extract, download_dir=DOWNLOAD_DIR)