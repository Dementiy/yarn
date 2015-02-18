#' Get synsets for a given word id (require registration)
#' 
#' @param word_id
#' @export
getSynsetsAPI <- function(word_id) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr package needed for this function to work. Please install it.", .call = FALSE)
  }
  
  query <- paste(domain, 'words/', word_id, '/synsets.xml', sep='')
  response <- GET(query)
  content(response, type='text/xml', encoding = 'UTF-8')
}

getWordsIdsFromSynset <- function(synset) {
  
}

# xmlChildren(getNodeSet(getSynsetsAPI(131), "//synsets/synset/words-ids")[[2]])

# GET('https://api.github.com', authenticate('Dementiy', '0verlo@d'))
# answer <- GET("http://russianword.net/users/auth/github?origin=http%3A%2F%2Frussianword.net%2F")
# content(answer, type="text/html")
# 
# oauth_endpoints('github')
# app <- oauth_app('github', '5aa330e6fe1d6e26e9e880bc3102ac6be3a4cf80')
# content(GET("http://russianword.net/words/131/synsets.xml?access_token=5aa330e6fe1d6e26e9e880bc3102ac6be3a4cf80"), type='text/xml', encoding='UTF-8')
# content(GET("http://russianword.net/words/131/synsets.xml"), type='text/xml', encoding='UTF-8')
# library(RCurl)
# getForm(uri = "https://github.com/login?return_to=%2Flogin%2Foauth%2Fauthorize%3Fclient_id%3Ddbdd26c6f57778150587%26redirect_uri%3Dhttp%253A%252F%252Frussianword.net%252Fusers%252Fauth%252Fgithub%252Fcallback%26response_type%3Dcode%26state%3Dd1e7478117c56570dc4cb2ef2f858f3b629f0344f3b0b5d8", .encoding = 'UTF-8')
# postForm(uri = "https://github.com/login", .opts=list(userpwd="Dementiy:0verlo@d", useragent = "RCurl"))
# ?curlOptions
# GET("http://russianword.net/users/auth/github?origin=http%3A%2F%2Frussianword.net%2F")
# GET("https://github.com/login?return_to=%2Flogin%2Foauth%2Fauthorize%3Fclient_id%3Ddbdd26c6f57778150587%26redirect_uri%3Dhttp%253A%252F%252Frussianword.net%252Fusers%252Fauth%252Fgithub%252Fcallback%26response_type%3Dcode%26state%3D4942d789e013e271e3aaee2e24cc289724e04a50aafe0a8a")
# answer <- GET("https://github.com/login?return_to=%2Flogin%2Foauth%2Fauthorize%3Fclient_id%3Ddbdd26c6f57778150587%26redirect_uri%3Dhttp%253A%252F%252Frussianword.net%252Fusers%252Fauth%252Fgithub%252Fcallback%26response_type%3Dcode%26state%3D2ea93eaa55c4574cb8119ed60b5d8794c38d745a2bd28248")
# content(answer, type='text/html')
# postForm(uri = 'https://github.com/login?return_to=%2Flogin%2Foauth%2Fauthorize%3Fclient_id%3Ddbdd26c6f57778150587%26redirect_uri%3Dhttp%253A%252F%252Frussianword.net%252Fusers%252Fauth%252Fgithub%252Fcallback%26response_type%3Dcode%26state%3D2ea93eaa55c4574cb8119ed60b5d8794c38d745a2bd28248',
#          .opts=list(userpwd='Dementiy:0verlo@d'))
# 
# cookie <- "MmJwckw1c2ZJZ25XMURjbmlPajZaODRxYU9FY3ZYQVMva1BSVEFNYXJoVHgzckJkcWNZUUw4UitlWGlJV25RVEVNU2grdUVPM0tTVEdqQ3J2cnI0dUo1Zk9NVE5Kbmd3QWdyNnF5cUtibkpCRm1oUXRqRmRHNzNhTm9mN0R4VmpPQUFZek01b0Fmalc1a1BVMGZLOTBGREpaYjV6MWpzbGFCVXk0ZmtWRmwyRlhvR0VhbEpEd3JQZHZWTnZJdUZvd2I4R0p4ZGo1M2IvUlF0c3ZDc0JlNWUxbjNkMHQ3UTdlU1lzd3BKSnZCOD0tLVFoTGNjU3BETk40dFhNQkdQNDU0UFE9PQ%3D%3D--afb147f9e69aedcdbb427162bad3c80429552a16"
# set_cookies('_yarn_session'='MmJwckw1c2ZJZ25XMURjbmlPajZaODRxYU9FY3ZYQVMva1BSVEFNYXJoVHgzckJkcWNZUUw4UitlWGlJV25RVEVNU2grdUVPM0tTVEdqQ3J2cnI0dUo1Zk9NVE5Kbmd3QWdyNnF5cUtibkpCRm1oUXRqRmRHNzNhTm9mN0R4VmpPQUFZek01b0Fmalc1a1BVMGZLOTBGREpaYjV6MWpzbGFCVXk0ZmtWRmwyRlhvR0VhbEpEd3JQZHZWTnZJdUZvd2I4R0p4ZGo1M2IvUlF0c3ZDc0JlNWUxbjNkMHQ3UTdlU1lzd3BKSnZCOD0tLVFoTGNjU3BETk40dFhNQkdQNDU0UFE9PQ%3D%3D--afb147f9e69aedcdbb427162bad3c80429552a16')
# content(GET("http://russianword.net/words/131/synsets.xml"), type='text/xml', encoding='UTF-8')
# 
#answer <- GET("http://russianword.net/words/131/synsets.xml", 
#               add_headers('Cookie'='_yarn_session=MmJwckw1c2ZJZ25XMURjbmlPajZaODRxYU9FY3ZYQVMva1BSVEFNYXJoVHgzckJkcWNZUUw4UitlWGlJV25RVEVNU2grdUVPM0tTVEdqQ3J2cnI0dUo1Zk9NVE5Kbmd3QWdyNnF5cUtibkpCRm1oUXRqRmRHNzNhTm9mN0R4VmpPQUFZek01b0Fmalc1a1BVMGZLOTBGREpaYjV6MWpzbGFCVXk0ZmtWRmwyRlhvR0VhbEpEd3JQZHZWTnZJdUZvd2I4R0p4ZGo1M2IvUlF0c3ZDc0JlNWUxbjNkMHQ3UTdlU1lzd3BKSnZCOD0tLVFoTGNjU3BETk40dFhNQkdQNDU0UFE9PQ%3D%3D--afb147f9e69aedcdbb427162bad3c80429552a16'))
# content(answer, type='text/xml', encoding='UTF-8')
# 
# cookies(x = '_yarn_session')

#getURL("http://russianword.net/words/131/synsets.xml", cookiefile="~/Library/Cookies/Cookies.binarycookies")
