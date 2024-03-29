**Please read this carefully as the download approach has been changed slightly.**


# deweydatayr
**_R_** library for Dewey Data Inc.

Find the release notes here: [Release Notes](https://github.com/Dewey-Data/deweydatar/blob/main/Release%20notes.md).

Bug report: https://community.deweydata.io/c/help/r/44.

Explore data at https://www.deweydata.io/.

Underlying Amplify API tutorial: https://github.com/amplifydata/amplifydata-public/blob/main/README.md

# Library tutorial

### 1. Create API Key
In the system, click Connections → Add Connection to create your API key.

<img src="https://github.com/Dewey-Data/deweydatar/assets/142400584/8ba869f0-3356-439c-8288-8b8d4bb76326" width="600">
<br>
<img src="https://github.com/Dewey-Data/deweydatar/assets/142400584/f479586b-ccf4-490b-852c-90051b1e7008" width="400">

As the message says, please make a copy of your API key and store it somewhere. Also, please hit the **Save** button before use.

### 2. Get a product path
Choose your product and Get / Subscribe → Connect to API then you can get API endpoint (product path). Make a copy of it.

<img src="https://github.com/Dewey-Data/deweydatar/assets/142400584/aef19ae4-eccd-4b58-a7f7-77ed698e0584" width="600">

### 3. Install `deweydatar` library
You can install this library directly from the GitHub source as following. It requires `devtools`.
```r
# Load "devtools" library (install it first, if don't have it)
library(devtools)
# Install deweydatar package from GitHub
install_github("Dewey-Data/deweydatar")

# Use deweydatar library
library(deweydatar)
```

`deweydatar` package has the following functions:
* `get_meta`: gets meta information of the datset, especially date range as returned in a `data.frame`
* `get_file_list`: gets the list of files in a `data.frame`
* `download_files`: download files from the file list to a destination folder
* `download_files0`: download files with apikey and product path to a destination folder
* `download_files1`: download files with apikey and product path to a destination folder (see below Examples for the difference between `download_files0` and `download_files1`)
* `read_sample`: read a sample of data for a file download URL
* `read_sample0`: read a sample of data for the first file with apikey and product path
* `read_local`: read data from locally saved csv.gz file

### 4. Examples
I am going to use Advan Weekly Pattern as an example.
```R
library(deweydatar)

# API Key
apikey_ = "Paste your API key from step 1 here."

# Advan product path
pp_advan_wp = "Paste product path from step 2 here."
```
You will only have one API Key while having different product paths for each product.

As a first step, check out the meta information of the dataset by
```R
meta = get_meta(apikey_, pp_advan_wp, print_meta = TRUE);
```
This will return a `data.frame` with meta information. `print_meta = TRUE` will print the meta information.
<img src="https://github.com/Dewey-Data/deweydatar/assets/142400584/e4d5f481-d807-4ba4-bf33-452032b661ed" width = "600">

You can see that the data has a partition column `DATE_RANGE_START`. Dewey data is usally huge and the data will be partitioned by this column into multiple files. We can also see that the minimum data available date is `2018-01-01` and maximum data available date is `2024-01-08`.
After checking this, I will download data between `2023-09-03` and `2023-12-31`.

Next, collect the list of files to download by
```r
files_df = get_file_list(apikey_, pp_advan_wp, 
                         start_date = '2023-09-03',
                         end_date = '2023-12-31',
                         print_info = T)
```
<br><br>
**Be careful!! ------------------------------------------------**

**For a selected date range, the download sever assigns file numbering (0, 1, 2, ...) for each file. Thus, if you have different date ranges (different `start_date` and `end_date`), file names will change due to the file numbering.**
**For example, the following `files_df1` and `files_df2` will have different file names due to different `start_date`.**
```r
files_df1 = get_file_list(apikey_, pp_advan_wp, 
                          start_date = '2023-09-03',
                          end_date = '2023-12-31',
                          print_info = T)

files_df2 = get_file_list(apikey_, pp_advan_wp, 
                          start_date = '2023-07-01',
                          end_date = '2023-12-31',
                          print_info = T)
```
**This also applies to the funtion `download_files0` and `download_files1` (demonstrated below) in the same way.**<br>
**------------------------------------------------------------**
<br><br>

If you do not specifiy `start_date`, it will collect all the files from the minimum available date, and do not spesify `end_date`, all the files to the maximum available date.

**Most Dewey datasets are very large. Please specify `start_date` and `end_date`.**

`print_info = TRUE` set to print another meta information of the files like below:
<img src="https://github.com/Dewey-Data/deweydatar/assets/142400584/897355b5-cf9f-4369-9969-bfef44942f28" width = "600">

files_df has a file links (`data.frame`) with the following information:
* `index`: file index ranges from 1 to the number of files
* `page`: page of the file
* `link`: file download link
* `partition_key`: to subselect files based on dates
* `file_name`
* `file_extension`
* `file_size_bytes`
* `modified_at`

Finally, you can download the data to a local destination folder by
```R
download_files(files_df, "C:/Temp", skip_exists = TRUE)
```
This will download files to `C:/Temp` directory, with the following progress messages.

<img src="https://github.com/Dewey-Data/deweydatar/assets/142400584/507b1e21-11d8-4231-943b-4c6b06f92ab5" width = "800">

If you attempt to download all the files again and want to skip already existing downloaded files, set `skip_exists = TRUE`. The default value is set to `FALSE` (the default value was `TRUE` in versions 0.1.x).

You can also use `filename_prefix` option to give file name prefix for all the files. For example, following will save all the files in the format of `advan_wp_xxxxxxx.csv.gz`.


```R
download_files(files_df, "K:/Temp", filename_prefix = "advan_wp_", skip_exists = T)
```

Alternatively, you can download files skipping `get_file_list` by
```R
download_files0(apikey_, pp_advan_wp, "K:/Temp",
                start_date = '2023-09-03', end_date = '2023-12-31')
```
or
```R
download_files1(apikey_, pp_advan_wp, "C:/Temp",
                start_date = '2023-09-03', end_date = '2023-12-31')
```
The difference between `download_files0` and `download_files1` is that `download_files0` collects all the file list (link) upfront and start downloading. As the links are valid for 24 hours, this may cause an interruption if the download takes over 24 hours. `download_files1`, on the other hand, collects a small page (group) of flie links and download them, and move on to the next page and download them, and so on. This helps the collected links to be valid while downloading. So, it is recommended to use `download_files1` for a large number of files that may take over 24 hours to download


Some datasets do not have partition column as they are time invariant (SafeGraph Global Places (POI) & Geometry, for example).
```R
meta = get_meta(apikey_, pp_sg_poipoly, print_meta = T);
```
<img src="https://github.com/Dewey-Data/deweydatar/assets/142400584/97671911-a476-496e-857c-7cb42bbd9a91" width = "600">

There is no partition column and minimum and maximum dates are not available. In that case, you can download the data without specifiying date range.
 
```R
files_df = get_file_list(apikey_, pp_sg_poipoly, print_info = T);
```
<br><br>
You can quickly load/see a sample data by
```R
sample_data = read_sample(files_df$link[1], nrows = 100)
```
This will load sample data for the first file in `files_df (files_df$link[1])` for the first 100 rows. You can see any files in the list.

You can also see the sample of the first file by
```R
sample_data = read_sample0(apikey_, pp_advan_wp, nrows = 100);
```
This will load the first 100 rows for the first file of Advan data.

You can open a downloaded local file (a csv.gz or csv file) by
```R
sample_local = read_local("C:/Temp/Weekly_Patterns_Foot_Traffic_Full_Historical_Data-0-DATE_RANGE_START-2023-09-04.csv.gz",
                          nrows = 100)
```
`read_local` function uses **_R_**’s base `read.csv` function. Files are large to read from local disk and `read.csv` can be slow. I recommend using `fread` function in the `data.table` package.
Thanks
