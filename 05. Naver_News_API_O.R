# ---- install (run once) ----
pkgs <- c("httr", "jsonlite", "wordcloud", "RColorBrewer")
new_pkgs <- setdiff(pkgs, rownames(installed.packages()))
if (length(new_pkgs)) install.packages(new_pkgs)

# ---- libraries ----
library(httr)
library(jsonlite)
library(wordcloud)
library(RColorBrewer)

# ---- Naver News API (JSON) ----
search_url   <- "https://openapi.naver.com/v1/search/news.json"
client_id    <- "gtgejjyDhDhcsx1D2Tjq"     # 본인 값으로 교체
client_secret<- "bm3LYc0dlk" # 본인 값으로 교체

query <- URLencode(enc2utf8("삼성전자"))
req_url   <- paste0(search_url, "?query=", query, "&display=100&start=1&sort=sim")

res <- GET(
  req_url,
  add_headers(
    "X-Naver-Client-Id"     = client_id,
    "X-Naver-Client-Secret" = client_secret
  )
)
stop_for_status(res)

dat <- fromJSON(content(res, as = "text", encoding = "UTF-8"))
df  <- dat$items
if (NROW(df) == 0) stop("검색 결과가 없습니다.")

# ---- text cleaning ----
txt <- df$description
txt <- gsub("<.*?>|&quot;|&amp;|&lt;|&gt;", " ", txt)
txt <- gsub("[[:digit:]]", " ", txt)
txt <- gsub("[^[:alnum:]가-힣\\s]", " ", txt)
txt <- gsub("\\s+", " ", txt)
txt <- trimws(enc2utf8(txt))

# ---- tokenize (fallback: whitespace) ----
tokens <- unlist(strsplit(txt, "\\s+"), use.names = FALSE)
tokens <- enc2utf8(tokens)
tokens <- tokens[nchar(tokens) >= 2 & nchar(tokens) <= 10]  # 2~10자만 사용

# ---- frequency ----
freq <- sort(table(tokens), decreasing = TRUE)
freq_df <- data.frame(term = names(freq), freq = as.integer(freq), stringsAsFactors = FALSE)

# ---- wordcloud (Plots 패널) ----
set.seed(1234)
wordcloud(
  words        = freq_df$term,
  freq         = freq_df$freq,
  min.freq     = 2,
  scale        = c(4, 0.8),
  rot.per      = 0.15,
  random.order = FALSE,
  random.color = TRUE,
  colors       = brewer.pal(8, "Dark2")
)

# ---- save PNG file ----
png("wordcloud.png", width = 1200, height = 800)
wordcloud(
  words        = freq_df$term,
  freq         = freq_df$freq,
  min.freq     = 2,
  scale        = c(4, 0.8),
  rot.per      = 0.15,
  random.order = FALSE,
  random.color = TRUE,
  colors       = brewer.pal(8, "Dark2")
)
dev.off()
