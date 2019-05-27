#### List of Datasources
1. [2016 Building Energy Benchmarking](https://data.seattle.gov/dataset/2016-Building-Energy-Benchmarking/2bpz-gwpy)
2. [King County Department of Assessments - Property tax valuation records](https://blue.kingcounty.com/Assessor/eRealProperty/default.aspx)
   * _10 digit Parcel Number_ is given in the benchmarking dataset
   * [Sample record](https://blue.kingcounty.com/Assessor/eRealProperty/Detail.aspx?ParcelNbr=1978201270)
   * [Code for downloading all these records](https://github.com/buds-lab/Seatle_benchmarking/blob/master/01_download_seatle_land_assesment_records.ipynb)
   * [Code for parsing building characteristics](https://github.com/buds-lab/Seatle_benchmarking/blob/master/01_parse_seatle_land_assesment_records.ipynb)

#### Other datasources (permit records)
1. [Building permits](https://data.seattle.gov/Permitting/Building-Permits/76t5-zqzr) - Demolition/alteration of building regions ([sample data](https://cosaccela.seattle.gov/portal/customize/LinkToRecord.aspx?altId=6086767-CN))
2. [Electrical permits](https://data.seattle.gov/Permitting/Electrical-Permits/c4tj-daue) - Installation/repair of electrical fittings ([sample data](https://cosaccela.seattle.gov/portal/customize/LinkToRecord.aspx?altId=6344600-EL))
3. [Land use permits](https://data.seattle.gov/Permitting/Land-Use-Permits/ht3q-kdvx) - Split up of land for different primary usage
