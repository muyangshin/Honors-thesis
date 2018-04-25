library(tidyverse)

source("code/naics.R")

# Kilcoyne
occs_high_tech <- c("11-302", "11-904", "11-912", "15-", "17-2", "17-3", "19-1", "19-2", "19-4", "27-1014", "29-2011",
                           "29-2033", "29-2034")

occs_tech_group <- c("15-", "17-", "19-", "29-")

# BLS
occs_bls_stem <- c("11-302", "11-904", "11-912", "15-", "17-102", "17-2", "17-3", "19-1", "19-2",
                          "19-301", "19-3022", "19-303", "19-304", "19-305", "19-309", "19-4", "41-903")

occs_bls_stem_related <- c(occs_bls_stem, "11-911", "17-", "29-")


ReadOES <- function(year, digits = 4, save = F) {
  # Read OES
  #
  # Args:
  #   year: 
  #
  #  Returns:
  #   data frame
  
  if (year >= 2008) {
    month.str <- "M"
  } else if (year == 2007) {
    month.str <- "May"
  } else if (year >= 2003) {
    month.str <- "may"
  } else {
    month.str <- ""
  }
  
  df <- read.csv(paste0("data/OES/", "nat", digits, "d_", month.str, year, "_dl.csv"), 
                 stringsAsFactors = F, encoding="UTF-8")
  
  colnames(df)[1] <- "NAICS"
  if (year <= 2002) {
    colnames(df) <- sapply(colnames(df), toupper)
  }
  
  df <- df %>% 
    mutate(
      TOT_EMP = ifelse(TOT_EMP == "**", NA, TOT_EMP),
      TOT_EMP = as.numeric(TOT_EMP)
      ) %>%
    filter(
      # drop government jobs
      NAICS < 999000,
      
      # drop empty tot_emp
      !is.na(TOT_EMP)
    )
  
  return(df)
}

CompileOES <- function(years, occupations, digits = 4, save = F, convert2002 = F) {
  # Read OES using the specified occupation grouping
  #
  # Args:
  #   years: single year / a vector of years
  #   occupations: vector
  #
  #  Returns:
  #   data frame
  #   year  NAICS empl  tech tech.pct tech.pct.economy selected
  
  ConvertTo2002NAICS_OES <- function(df, year) {
    if (year >= 2008) {
      if (year >= 2017) {
        conversion.list <- naics.2017.to.2012
      } else if (year >= 2012) {
        conversion.list <- naics.2012.to.2007
      } else {
        conversion.list <- naics.2007.to.2002
      }
  
      df2 <- df %>%
        filter(NAICS %in% names(conversion.list)) %>%
        mutate(old = TRUE)
  
      df3 <- df2
      for (i in 1:nrow(df2)) {
        old.naics <- as.character(df2[i, "NAICS"])
        for (j in seq_along(names(conversion.list[[old.naics]]))) {
          this.naics = names(conversion.list[[old.naics]])[j]
          this.prop = conversion.list[[old.naics]][[j]]
  
          df3 <- df3 %>%
            rbind(list(
              this.naics,
              as.numeric(this.prop * df2[i, "empl"]),
              as.numeric(this.prop * df2[i, "tech"]),
              FALSE
            ))
        }
      }
  
      df <- df3 %>%
        filter(old == FALSE) %>%
        select(-old) %>%
        rbind(df %>% filter(!NAICS %in% df2$NAICS)) %>%
        group_by(NAICS) %>%
        summarise(empl = sum(empl),
                  tech = sum(tech)
        )
  
      # If it uses 2002 NAICS, needs to be converted twice
      if (year >= 2017) {
        df <- ConvertTo2002NAICS_OES(df, 2012)
      } else if (year >= 2012) {
        df <- ConvertTo2002NAICS_OES(df, 2008)
      }
    }
    
    return(df)
  }
  
  
  if (length(years) > 1) {
    # Multiple Years
    df <- CompileOES(years[1], occupations, digits = digits, convert2002 = convert2002)
    
    for (year in tail(years, -1)) {
      df <- rbind(df, CompileOES(year, occupations, digits = digits, convert2002 = convert2002))
    }
    
    if (save) {
      write.csv(df, "data_out/OES_by_industry_total.csv")
    }  
    
    return(df)
    
  } else {
    # Single Year
    year <- years
    
    # Read --------------------------------------------------------------------
    
    df <- ReadOES(year, digits)
    
    # is.subset ---------------------------------------------------------------
    
    # Filter out OCC_CODE that ends with 0: leave only "detailed"
    df <- df %>% 
      #is.subset
      filter(!grepl("0+$", OCC_CODE)) %>%
      mutate(is.subset = grepl(paste0("^", paste0(occupations, collapse = "|")), OCC_CODE)) %>%
      mutate(tech = TOT_EMP * is.subset) %>%
    
      # Collapse by NAICS
      group_by(NAICS) %>%
      summarise(empl = sum(TOT_EMP), 
                tech = sum(tech)
      )
    
    # NAICS conversion
    if (digits == 4 & convert2002) {
      if (year <= 2008) {
        df <- ConvertTo2002NAICS_OES(df, year)
      }
    }
    
    
    # fractions ---------------------------------------------------------------
    df <- df %>%
      mutate(tech.pct = tech / empl, 
             tech.pct.economy = sum(tech) / sum(empl),
             tech.pct.median = median(tech.pct),
             tech.pct.sd = sd(tech.pct),
             # selected = ifelse(tech.pct >= 2 * tech.pct.economy, 1, 0),
             year = year,
             NAICS = as.character(NAICS)) %>%
      
      # year as the first column
      select(year, everything())
    
    return(df)
  }
}
