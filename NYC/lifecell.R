url = "http://104.211.245.197/report/179689_CBDUAL.pdf"

url_start = "http://104.211.245.197/report/"
url_suffix  = "_CBDUAL.pdf"

id1 = 179689
id2 = 179689 + 10

for (id in id1:id2) {
  try({
    to_file = paste0(id, url_suffix)
    url0 = paste0(url_start, to_file)
    download.file(url0, to_file)
    print(url0)
  })
}

download.file(url, "report.pdf")