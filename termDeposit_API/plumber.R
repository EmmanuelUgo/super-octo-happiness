#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

library(plumber)
library(tidyverse)
library(tidymodels)
library(scales)

model <- read_rds("term_deposit_model.rds")




#* @apiTitle Customer Churn API
#* @apiDescription A Simple API to predict whether a customer would subscribe for a new term deposit
#* @apiVersion  1.0.1


#* Health Check - Is the API working?
#* @get /echo
function() {
    list(msg = "todo bien",
         time = Sys.time())
}

#* Term Deposit Probability
#* @param a Age
#* @param b Job
#* @param c Marital
#* @param d Education
#* @param e Default (yes or no)
#* @param f Balance
#* @param g Housing (yes or no)
#* @param h Loan (yes or no)
#* @param i Contact
#* @param j Day (day in a month)
#* @param k Month
#* @param l Duration
#* @param m Campaign
#* @param n Pdays
#* @param o Previous
#* @param p Poutcome

#* @response Job Jobs are: unemployed, services, management, blue-collar, self-employed,technician, entrepreneur, admin, student, housemaid, retired.
#* @response Contact Contacts are: cellular, unknown, telephone
#* @response Month Months in jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec
#* @response Poutcome Poutcome in unknown,failure,other,success
#* @response Marital married,single,divorced
#* @response Education primary,secondary,tertiary
#* @serializer png
#* @get /predict
function(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) {
  
   df <- tibble(
      age =as.numeric(a), job = as.character(b),
      marital = as.character(c), education =  as.character(d),
      default = as.character(e), balance = as.numeric(f),
      housing = as.character(g), loan = as.character(h),
      contact = as.character(i), day = as.numeric(j),
      month = as.character(k), duration = as.numeric(l),
      campaign = as.numeric(m), pdays = as.numeric(n),
      previous = as.numeric(o), poutcome = as.character(p)
    ) %>%
     mutate_if(is.character,str_to_lower)
   
    
    
    
   prob_result <- predict(model, df, type = "prob")
    
    
   prob_plot <- prob_result %>%
    gather() %>%
    mutate(key = str_sub(key, start = 7, end = 10) %>% str_to_sentence()) %>%
    ggplot(aes(key, value, fill = key)) +
    geom_col(show.legend = F) +
    scale_y_continuous(labels = percent_format()) +
    scale_fill_manual(values = c("#6E0808", "#179944"))+
    labs(title = "Probability Plot",
         x = NULL,
         y = NULL,
         caption = "plot: @EmmanuelUgo | data: NHJL Bank")+
     theme_minimal()
    
    print(prob_plot)
    

    }

