source("code/tools.R")


# CPS ACES ----------------------------------------------------------------

# 2002 to 2017
write_ipums_to_sql(cps_data_file = "data/CPS/cps_aces_02_to_17.dat",
                   cps_ddi_file = "data/CPS/cps_aces_02_to_17.xml",
                   out = "data_out/cps_aces_02_to_17.sqlite")

# 1992 to 2001: append
write_ipums_to_sql(cps_data_file = "data/CPS/cps_aces_92_to_01.dat",
                   cps_ddi_file = "data/CPS/cps_aces_92_to_01.xml",
                   out = "data_out/cps_aces.sqlite",
                   append = T)

# # cps org
# # 1994
# write_ipums_to_sql(cps_data_file = "data/CPS/cps_org_94.dat",
#                    cps_ddi_file = "data/CPS/cps_org_94.xml",
#                    out = "data_out/cps_org_94.sqlite")
# 
# # 2002, 2016
# write_ipums_to_sql(cps_data_file = "data/CPS/cps_org.dat",
#                    cps_ddi_file = "data/CPS/cps_org.xml",
#                    out = "data_out/cps_org.sqlite")
# 
# # 2002 to 2009
# write_ipums_to_sql(cps_data_file = "data/CPS/cps_org_02_to_09.dat",
#                    cps_ddi_file = "data/CPS/cps_org_02_to_09.xml",
#                    out = "data_out/cps_org_02_to_09.sqlite")
# 
# # 2010 to 2016
# write_ipums_to_sql(cps_data_file = "data/CPS/cps_org_10_to_16.dat",
#                    cps_ddi_file = "data/CPS/cps_org_10_to_16.xml",
#                    out = "data_out/cps_org_10_to_16.sqlite")