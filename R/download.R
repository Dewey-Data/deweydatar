#' @exportPattern "^[[:alpha:]]+"
#'
#' @import httr2
library(httr2)

# make api endpoint from product ID
make_api_endpoint = function(path) {
  # py_update
  # remove trailing spaces
  path = trimws(path)
  # py_update_

  if(!startsWith(path, "https://")) {
    api_endpoint = paste0("https://app.deweydata.io/external-api/v3/products/", path, "/files")
    return(api_endpoint)
  } else {
    return (path)
  }
}

#' @title Get metadata from server
#' @description
#' Get metadata from server
#' @param apikey (character) API Key.
#' @param product_path (character) API endpoint or Product ID.
#' @param print_meta (logical) Print metadata. Default is TRUE.
#' @details
#' None
#' @return A data.frame object contains metadata.
#'  \item{total_files}{total number of files.}
#'  \item{total_size_MB}{total size of files in MB.}
#'  \item{partition_column}{data partition column name.}
#'  \item{partition_aggregation}{aggregation frequency.}
#'  \item{min_partition_key}{min date available.}
#'  \item{max_partition_key}{max date available.}
#' @author Dewey Data Inc.
#' @references
#' None
#' @examples
#' # not run
#' meta_df = get_meta(apikey_, product_path_, print_meta = TRUE)
#'
#' @export
get_meta = function(apikey, product_path, print_meta = TRUE) {
  product_path = make_api_endpoint(product_path)
  req = request(paste0(product_path,"/metadata")) %>%
    req_headers("X-API-KEY" = apikey) %>%
    req_headers("accept" = "application/json")

  response = tryCatch(
    {
      req_perform(req)
    }, warning = function(cond) {
      message("Warning during httr2::req_perform.")
      message(cond)
      message("")
    }, error = function(cond) {
      message("Error during httr2::req_perform.")
      message(cond)
      message("")
    }
  )

  if(is.null(response)) {
    return(NULL)
  } else if(response$status_code == 401) {
    print(response);
    return(NULL);
  }

  # resp_content_type(response)
  # resp_status_desc(response)

  res_json = resp_body_json(response)

  if(!('total_files' %in% names(res_json))) {
    print("Error in response.json")
    print(res_json)
    print(" ")

    return(NULL)
  }

  # Convert res_json list to a data.frame
  meta = data.frame(t(unlist(res_json)))

  # total_files as numeric
  meta$total_files = as.numeric(meta$total_files)
  # total_size as numeric
  meta$total_size = as.numeric(meta$total_size)

  # total_size is in megabytes
  meta$total_size = meta$total_size/1000000

  # change column name to total_size_GB
  colnames(meta)[colnames(meta) == "total_size"] = "total_size_MB"


  if(print_meta == TRUE) {
    message(" ")
    message("Metadata summary ------------------------------------------------")
    message(paste0("Total number of files: ", format(meta$total_files, big.mark = ",")))
    message(paste0("Total files size (MB): ", format(round(meta$total_size_MB, digits = 2), big.mark = ",")))
    message(paste0("Date aggregation: ", meta$partition_aggregation))
    message(paste0("Date partition column: ", meta$partition_column))
    message(paste0("Data min available date: ", meta$min_partition_key))
    message(paste0("Data max available date: ", meta$max_partition_key))
    message("-----------------------------------------------------------------")
    message(" ")
  }

  return(meta)
}

# py_update
print_selection_meta = function(selection_meta, pages_meta) {
  message("Files information summary ---------------------------------------")
  message(paste0("Total number of pages: ", format(selection_meta$total_pages, big.mark = ",")))
  message(paste0("Total number of files: ", format(selection_meta$total_files, big.mark = ",")))
  message(paste0("Total files size (MB): ", format(round(selection_meta$total_size_MB, digits = 2), big.mark = ",")))
  message(paste0("Average single file size (MB): ",
                 format(round(mean(pages_meta$avg_file_size_for_page), digits = 2), big.mark = ",")))
  message(paste0("Date partition column: ", pages_meta$partition_column[1]))
  message(paste0("Expires at: ", selection_meta$expires_at))
  message("-----------------------------------------------------------------")
}
# py_update_

#' @title
#' Collects file list from server
#' @description
#' Collects the file list information from data server.
#' @param apikey (character) API Key.
#' @param product_path (character) API endpoint or Product ID.
#' @param start_page (integer) Start page of file list. Default is 1.
#' @param end_page (integer) End page of file list. Default is Inf.
#' @param start_date (character) Data start date character for files in the form of "2021-07-01".
#' Default is NULL ("1000-01-01"), which indicates no limit.
#' @param end_date (character) Data end date character for files in the form of "2023-08-21".
#' Default is NULL ("9999-12-31"), which indicates no limit.
#' @param meta (data.frame) Metadata collected from \code{\link{get_meta}}.
#' If \code{meta} is NULL, \code{\link{get_meta}} will be called.
#' @param print_info (logical) Print file list information. Default is TRUE.
#' @details
#' None
#' @return A data.frame object contains files information.
#'   \item{index}{file index starts from 1.}
#'   \item{page}{page of the file list.}
#'   \item{link}{download link.}
#'   \item{partition_key}{date field to partiton files.}
#'   \item{file_name}{file name.}
#'   \item{file_extension}{file extension (e.g. .csv.gz.)}
#'   \item{download_lik}{same as \code{link}.}
#' @author Dewey Data Inc.
#' @references
#' None
#' @seealso
#' \code{\link{slice_files_df}}, \code{\link{read_sample_data}}
#' @examples
#' # not run
#' files_df = get_file_list(apikey_, product_path_,
#'            start_page = 1, end_page = 3, print_info = T)
#'
#' @export
get_file_list = function(apikey, product_path,
                         start_page = 1, end_page = Inf,
                         start_date = NULL, end_date = NULL,
                         meta = NULL,
                         print_info = T) {
  product_path = make_api_endpoint(product_path)

  if(is.null(meta)) {
    meta = get_meta(apikey, product_path, print_meta = F)
  }

  selection_meta = NULL
  pages_meta = NULL
  files_df = NULL

  if(is.null(start_date)) {
    start_date = "1000-01-01"
  }
  if(is.null(end_date)) {
    end_date = "9999-12-31"
  }

  # To proper date format: for example '2023-3-4' to '2023-03-04'
  start_date = as.character(as.Date(start_date))
  end_date = as.character(as.Date(end_date))

  # print(start_date)
  # print(end_date)

  page = start_page
  while(T) {
    req = request(product_path) %>%
      req_headers("X-API-KEY" = apikey) %>%
      req_headers("accept" = "application/json") %>%
      req_url_query ("page" = page)

    # Only datasets with partition column
    # if("partition_column" %in% names(meta)) {
    if(!is.null(meta$partition_column)) {
      req = req %>%
      req_url_query ("partition_key_after" = start_date) %>%
      req_url_query ("partition_key_before" = end_date)
    }

    response = tryCatch(
      {
        req_perform(req)
      }, warning = function(cond) {
        message("Warning during httr2::req_perform.")
        message(cond)
        message("")
      }, error = function(cond) {
        message("Error during httr2::req_perform.")
        message(cond)
        message("")
      }
    )

    if(is.null(response)) {
      return(NULL)
    } else if(response$status_code == 401) {
      message(response);
      return(NULL);
      # py_update
    } else if(response$status_code == 422) {
      message(response);
      return(NULL);
    }
    # py_update_

    # resp_content_type(response)
    # resp_status_desc(response)

    res_json = resp_body_json(response)

    if(is.null(res_json$page)) {
      message("Error in resp_body_json")
      message(res_json)
      message(" ")
      return(NULL)
    }

    # initialize
    if(res_json$page == start_page) {
      # Convert res_json list to a data.frame
      selection_meta = data.frame(
        total_files = res_json$total_files,
        total_pages = res_json$total_pages,
        total_size_MB = res_json$total_size/1000000,
        expires_at = res_json$expires_at
        )
    }

    if(print_info == T) {
      message(paste0("Collecting files information for page ", res_json$page, "/",
                     res_json$total_pages, "..."))
    }

    if(!is.null(res_json$partition_column)) {
      partition_column = res_json$partition_column
    } else {
      partition_column = NA
    }

    pages_meta = rbind(pages_meta,
                      data.frame(page = res_json$page,
                                 number_of_files_for_page = res_json$number_of_files_for_page,
                                 avg_file_size_for_page = res_json$avg_file_size_for_page/1000000,
                                 partition_column = partition_column)
                      )

    col_names_str = names(res_json$download_links[[1]])
    dn_link_unlist = unlist(res_json$download_links)

    # When partition column (key) is null
    if(!is.null(res_json$partition_column)) {
      dn_link_df = data.frame(matrix(dn_link_unlist, ncol = length(col_names_str), byrow = T))
    } else {
      dn_link_df = data.frame(matrix(dn_link_unlist, ncol = length(col_names_str)-1, byrow = T))
      dn_link_df = data.frame(dn_link_df[, 1], NA, dn_link_df[2:ncol(dn_link_df)])
    }
    colnames(dn_link_df) = col_names_str

    page_files_df = data.frame(page = res_json$page, dn_link_df)

    files_df = rbind(files_df, page_files_df)

    page = res_json$page + 1

    if((page > res_json$total_pages) | (page > end_page)) {
      if(print_info == T) {
        message("Files information collection completed.")
      }

      break
    }
  }

  # To date type
  # Account for if partition_key is in "YYYY-MM" format, append "-01" for new platform
  files_df$partition_key = ifelse(
    grepl("^\\d{4}-\\d{2}$", files_df$partition_key),
    paste0(files_df$partition_key, "-01"),
    files_df$partition_key
  )
  files_df$partition_key = as.Date(files_df$partition_key)

  # Attach index
  files_df = data.frame(index = 1:nrow(files_df), files_df)
  # Backward compatibility
  files_df$download_link = files_df$link

  if(print_info == T) {
    message(" ")
    print_selection_meta(selection_meta, pages_meta);
    # message("Files information summary ---------------------------------------")
    # message(paste0("Total number of pages: ", format(selection_meta$total_pages, big.mark = ",")))
    # message(paste0("Total number of files: ", format(selection_meta$total_files, big.mark = ",")))
    # message(paste0("Total files size (MB): ", format(round(selection_meta$total_size_MB, digits = 2), big.mark = ",")))
    # message(paste0("Average single file size (MB): ",
    #                format(round(mean(pages_meta$avg_file_size_for_page), digits = 2), big.mark = ",")))
    # message(paste0("Date partition column: ", pages_meta$partition_column[1]))
    # message(paste0("Expires at: ", selection_meta$expires_at))
    # message("-----------------------------------------------------------------")
    message(" ")
  }

  #return(list(files_df = files_df,
  #            selection_meta = selection_meta, pages_meta = pages_meta));

  # Attache selection_meta and pages_meta to files_df as atrributes
  attr(files_df, "selection_meta") = selection_meta
  attr(files_df, "pages_meta") = pages_meta

  return(files_df)
}

#' @title
#' Read sample data into memory from a URL
#' @description
#' Read sample data into memory from a URL.
#' @param url (character) A file URL.
#' @param nrows (integer) Number of rows to read. Default is 100.
#' @details
#' None
#' @return A data.frame object contains data.
#' @author Dewey Data Inc.
#' @references
#' None
#' @seealso
#' \code{\link{get_file_list}}, \code{\link{read_sample_data0}}
#' @examples
#' # not run
#' sample_data = read_sample(files_df$link[1], nrows = 100)
#'
#' @export
read_sample = function(url, nrows = 100) {
  # if(nrows > 1000) {
  #   message("Warning: set nrows no greater than 1000.");
  #   nrows = 1000;
  # }

  url_con = gzcon(url(url), text = T);
  df = read.csv(url_con, nrows = nrows, skipNul = TRUE, encoding = "UTF-8");

  return(df);
}

# Backward compatibility
#' @export
read_sample_data = read_sample

#' @title
#' Read sample data into memory from a URL
#' @description
#' Read first file data into memory using API key and product path.
#' @param apikey (character) API key.
#' @param product_path (character) API endpoint.
#' @param nrows (integer) Number of rows to read. Default is 100.
#' @details
#' None
#' @return A data.frame object contains data.
#' @author Dewey Data Inc.
#' @references
#' None
#' @seealso
#' \code{\link{get_file_list}}, \code{\link{read_sample_data}}
#' @examples
#' # not run
#' sample_data0 = read_sample0(apikey_, pp_advan_wp, 100)
#'
#' @export
read_sample0 = function(apikey, product_path, nrows = 100) {
  files_df = get_file_list(apikey, product_path,
                           start_page = 1, end_page = 1, print_info = F);
  message("    ");

  if(!is.null(files_df) & (nrow(files_df) > 0)) {
    return(read_sample(files_df$link[1], nrows));
  }
}

# Backward compatibility
#' @export
read_sample_data0 = read_sample0

#' @title
#' Download files from file list to a destination folder
#' @description
#' Download files from file list collected from \code{\link{get_file_list}}
#' to a destination folder
#' @param files_df (data.frame) File list collected from \code{\link{get_file_list}}.
#' @param dest_folder (character) Destination local folder to save files.
#' @param filename_prefix (character) Prefix for file names.
#' @param skip_exists (boolean) Skips downloading if the file
#' @details
#' The file links from  \code{\link{get_file_list}} are valid for 24 hours.
#' If download process passes the 24 hours period,
#' the \code{files_df} will be no longer valid
#' and \code{\link{get_file_list}} must be called again to collect new
#' \code{files_df}. Then run this function again with \code{skip_exists = TRUE} option.
#' @return None.
#' @author Dewey Data Inc.
#' @references
#' None
#' @seealso
#' \code{\link{download_files0}}, \code{\link{download_files1}}
#' @examples
#' # not run
#' download_files(files_df, "C:/temp", "advan_wp_", skip_exists = FALSE)
#'
#' @export
download_files = function(files_df, dest_folder,
                          filename_prefix = NULL, skip_exists = FALSE) {
  dest_folder = gsub("\\", "/", dest_folder, fixed = T);

  if(!endsWith(dest_folder, "/")) {
    dest_folder = paste0(dest_folder, "/");
  }

  # total_pages from files_df
  total_pages = attr(files_df, "selection_meta")$total_pages[1]
  # number of files
  num_files = nrow(files_df);

  for (i in 1:num_files) {
    message(paste0("Downloading file ", i, "/", num_files, " (file index = ", files_df$index[i], ")",
                   ", page ", files_df$page[i], "/", total_pages)
            )

    file_name = paste0(filename_prefix, files_df$file_name[i]);

    dest_path = paste0(dest_folder, file_name)
    if (file.exists(dest_path) && skip_exists) {
      message(paste0("File already exists: ", dest_path))
      message("Skipping...")

      next
    }

    message(paste0("Writing ", dest_path))
    message("Please be patient. It may take a while...")

    req = request(files_df$link[i])
    response = req_perform(req)

    file_con = file(dest_path, "wb")
    writeBin(response$body, file_con)
    close(file_con)

    message("   ")
  }
}

#' @title
#' Download files with API key and product path to a destination folder
#' @description
#' Download files with API key and product path to a destination folder.
#' @param apikey (character) API Key.
#' @param product_path (character) API endpoint or Product ID.
#' @param dest_folder (character) Destination local folder to save files.
#' @param start_date (character) Data start date character for files in the form of "2021-07-01".
#' Default is NULL ("1000-01-01"), which indicates no limit.
#' @param end_date (character) Data end date character for files in the form of "2023-08-21".
#' Default is NULL ("9999-12-31"), which indicates no limit.
#' @param filename_prefix (character) Prefix for file names.
#' @param skip_exists (boolean) Skips downloading if the file exists. Default is FALSE.
#' @details
#' The file links inside this function are valid for 24 hours.
#' If download process passes the 24 hours period,
#' this function must be called again with \code{skip_exists = TRUE} option.
#' When file download will take over 24 hours, use \code{\link{download_files1}}.
#' @return None.
#' @author Dewey Data Inc.
#' @references
#' None
#' @seealso
#' \code{\link{download_files}}, \code{\link{download_files1}}
#' @examples
#' # not run
#' download_files0(apikey_, product_path, "C:/temp")
#'
#' @export
download_files0 = function(apikey, product_path, dest_folder,
                           start_date = NULL, end_date = NULL,
                           filename_prefix = NULL, skip_exists = FALSE) {
  #files_df = get_file_list(apikey, product_path, print_info = T);
  files_df = get_file_list(apikey = apikey, product_path = product_path,
                           start_page = 1, end_page = Inf,
                           start_date = start_date, end_date = end_date,
                           print_info = T);
  # py_update
  # message(" ");
  message("Start downloading...")
  message(" ")
  # py_update_

  if(!is.null(files_df) & (nrow(files_df) > 0)) {
    download_files(files_df, dest_folder, filename_prefix, skip_exists);
  } else {
    message("No files to download.")
  }

  # py_update
  message(" ")
  message("Download completed.");
  # py_update_
}

#' @title
#' Download files with API key and product path to a destination folder
#' @description
#' Download files with API key and product path to a destination folder.
#' @param apikey (character) API Key.
#' @param product_path (character) API endpoint or Product ID.
#' @param dest_folder (character) Destination local folder to save files.
#' @param start_date (character) Data start date character for files in the form of "2021-07-01".
#' Default is NULL ("1000-01-01"), which indicates no limit.
#' @param end_date (character) Data end date character for files in the form of "2023-08-21".
#' Default is NULL ("9999-12-31"), which indicates no limit.
#' @param filename_prefix (character) Prefix for file names.
#' @param skip_exists (boolean) Skips downloading if the file exists. Default is FALSE.
#' @details None.
#' @return None.
#' @author Dewey Data Inc.
#' @references
#' None
#' @seealso
#' \code{\link{download_files}}, \code{\link{download_files0}}
#' @examples
#' # not run
#' download_files1(apikey_, product_path, "C:/temp")
#'
#' @export
download_files1 = function(apikey, product_path, dest_folder,
                           start_date = NULL, end_date = NULL,
                           filename_prefix = NULL, skip_exists = FALSE) {
  # Get meta data
  meta = get_meta(apikey, product_path, print_meta = F)

  # Call get_file_list with meta for the first page to see the total_pages
  p1_files_df = get_file_list(apikey = apikey, product_path = product_path,
                           start_page = 1, end_page = 1,
                           start_date = start_date, end_date = end_date,
                           meta = meta,
                           print_info = F);
  print_selection_meta(attr(p1_files_df, "selection_meta"), attr(p1_files_df, "pages_meta"))

  selection_meta = attr(p1_files_df, "selection_meta")

  message(" ");
  message("Start downloading...")
  for(i in 1:selection_meta$total_pages[1]) {
    message(" ")
    message(paste0("Downloading page ", i, "/", selection_meta$total_pages[1], "..."))

    files_df = get_file_list(apikey = apikey, product_path = product_path,
                             start_page = i, end_page = i,
                             start_date = start_date, end_date = end_date,
                             meta = meta,
                             print_info = F);

    download_files(files_df, dest_folder, filename_prefix, skip_exists);
  }
  message(" ")
  message("Download completed.");
}

#' @title
#' Slice files_df from \code{get_file_list}
#' @description
#' Slice \code{files_df} from \code{get_file_list} for specific data range
#' of from start_date to end_date.
#' For example, start_date = "2023-08-14", end_date = "2023-08-21".
#' @param files_df (data.frame) \code{files_df} from \code{get_file_list}.
#' @param start_date (character) Start date character for files in the form of "2023-08-21".
#' @param end_date (character) End date character for files in the form of "2023-08-21".
#' Default is \code{NULL}, which indicates slice to the last date.
#' @details
#' None
#' @return None.
#' @author Dewey Data Inc.
#' @references
#' None
#' @seealso
#' \code{\link{get_file_list}}
#' @examples
#' # not run
#' sliced_files_df = slice_files_df(files_df, "2023-09-01", "2023-09-10")
#'
#' @export
slice_files_df = function(files_df, start_date, end_date = NULL) {
  start_date = as.Date(start_date)
  if(is.null(end_date)) {
    end_date = Inf
  }
  end_date = as.Date(end_date)

  sliced_df = files_df[(start_date <= files_df$partition_key) &
                         (files_df$partition_key <= end_date), ]
  return (sliced_df)
}

#' @title
#' Read local data into memory from a path
#' @description
#' Read local data into memory from a path
#' @param apikey (character) Path to a .csv.gz or csv file.
#' @param nrows (integer) Number of rows to read. Default is -1 (all).
#' @details
#' This can be slow. Recommend to use \code{fread} function
#' at \code{data.table} package.
#' @return A data.frame object contains data.
#' @author Dewey Data Inc.
#' @references
#' None
#' @seealso
#' None
#' @examples
#' # not run
#' local_data = read_local(path, nrows = 100)
#'
#' @export
read_local = function(path, nrows = -1) {
  tryCatch({
    df = read.csv(gzfile(path), nrows = nrows);
  }, error = function(cond) {
    tryCatch({
      df = fread(path, nrows = nrows)
    }, error = function(cond) {
      message("File type not supported. Only zipped csv or csv files are supported.")
    })
    df = read.csv(path, nrows = nrows)
  })
  df = read.csv(gzfile(path), nrows = nrows);

  return(df);
}

# Backward compatibility
#' @export
read_local_data = read_local

