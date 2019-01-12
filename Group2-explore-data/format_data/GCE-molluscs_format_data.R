library(popler)
library(rvest)

# get mollusc data links from popler
mollusc_links <- pplr_browse(proj_metadata_key == 298,
                             full_tbl=T)$metalink %>% 
                    strsplit(';')

# links to dois
get_dois <- function(x, node_str){
  
  read_html(x) %>%
    html_nodes(node_str) %>%       # find all links
    html_text()
  
}

# open the URLs
gce_dois <- mollusc_links %>%
              # remove white spaces
              sapply(trimws) %>% 
              # get DOIS
              sapply( get_dois, 'tr:nth-child(5) a' ) %>% 
              # paste actual DOIs
              sapply(function(x) paste0( 'http://dx.doi.org/',x) ) %>% 
              # 
              setNames( NULL )

sapply(gce_dois, browseURL)

'https://pasta.lternet.edu/package/data/eml/knb-lter-gce/459/4/ab7d98b17f4eed0ca9cb7387230a2220'
