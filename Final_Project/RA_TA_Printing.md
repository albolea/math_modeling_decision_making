---
title: "RA_TA_Printing"
author: "Renato Albolea"
date: "6/8/2020"
output: 
  html_document: 
    keep_md: yes
    toc: yes
    toc_float: true
    toc_depth: 4
    code_download: yes
---




# Objective:  

Improve the printing process to prevent bottlenecks and reduce work in progress(WIP) costs. This technical appendix brings support for 2 proposals of how to improve the process.  
\  

# General Characteristics:  

+ Equipments:  
  - 10 identical printers;  
  - average of 1 print per hour;  
  - exponential distributed work time;
  
+ Poster sheets arrival to the printer group:   
  - Randomly;  
  - 7 sheets per hour;
  
+ Inspections:
  - 8 inspections per hour per person;  
  - arrival: 7 per hour;
  
+ Cost: 
  - $ 8.00 per hour for each poster in progress;   
  - $ 7.00 per hour for running each press;  
  - $17.00 per hour for junior inspector;  


# Status Quo






## Printers:   

```r
# Parameters
p_lam <- 7
p_mu <-  1
p_s <- 10

p_rho <- p_lam/(p_mu*p_s)

# Probability of 0 prints
p_p0 <- 1 - p_rho
```

+ A printer is expected to be busy 70% of times



```r
# Set up problem with criteria and 7 poster papers
n <- 20

p_sqo <- queue_model(lambda = p_lam,
                     mu = p_mu,
                     s = p_s,
                     n = n,
                     queue_cost = 8,
                     work_cost = 7,
                     use_fixed_cost = FALSE,
                     service_name = "Printers")

p_sqo$html_tab
```

<table class="table table-striped table-bordered" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> # Printers </th>
   <th style="text-align:right;"> L </th>
   <th style="text-align:right;"> Lq </th>
   <th style="text-align:right;"> W (minutes) </th>
   <th style="text-align:right;"> Wq (minutes) </th>
   <th style="text-align:right;"> Queue Cost </th>
   <th style="text-align:right;"> In-proccess Cost </th>
   <th style="text-align:right;"> Service Cost </th>
   <th style="text-align:right;"> Total Cost </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 7.52 </td>
   <td style="text-align:right;"> 0.52 </td>
   <td style="text-align:right;"> 64.43 </td>
   <td style="text-align:right;"> 4.43 </td>
   <td style="text-align:right;"> 4.14 </td>
   <td style="text-align:right;"> 60.14 </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:right;"> 109.14 </td>
  </tr>
</tbody>
<tfoot>
<tr>
<td style = 'padding: 0; border:0;' colspan='100%'><sup>*</sup> Probability pritings will wait in the queue 22.2%</td>
</tr>
</tfoot>
<tfoot>
<tr>
<td style = 'padding: 0; border:0;' colspan='100%'><sup>*</sup> Probability allPrinters are busy is 7.9%</td>
</tr>
</tfoot>
</table>

## Inspection:   

```r
# Parameters
i_lam <- 7
i_mu <-  8
i_s <- 1

i_rho <- i_lam/(i_mu*i_s)

# Probability of 0 prints
i_p0 <- 1 - i_rho
```

+ The inspector is expected to be busy 87.5% of times



```r
# Set up problem with criteria and 20 inspections
n <- 20

i_sqo <- queue_model(lambda = i_lam,
                     mu = i_mu,
                     s = i_s,
                     n = n,
                     queue_cost = 8,
                     work_cost = 17,
                     use_fixed_cost = TRUE,
                     service_name = "Inspectors")

sqo_total_cost <- i_sqo$raw_tab$`Total Cost` + p_sqo$raw_tab$`Total Cost`
sqo_queue_cost <- i_sqo$raw_tab$`Queue Cost` + p_sqo$raw_tab$`Queue Cost`

i_sqo$html_tab
```

<table class="table table-striped table-bordered" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> # Inspectors </th>
   <th style="text-align:right;"> L </th>
   <th style="text-align:right;"> Lq </th>
   <th style="text-align:right;"> W (minutes) </th>
   <th style="text-align:right;"> Wq (minutes) </th>
   <th style="text-align:right;"> Queue Cost </th>
   <th style="text-align:right;"> In-proccess Cost </th>
   <th style="text-align:right;"> Service Cost </th>
   <th style="text-align:right;"> Total Cost </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 6.12 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:right;"> 52.5 </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:right;"> 56 </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 73 </td>
  </tr>
</tbody>
<tfoot>
<tr>
<td style = 'padding: 0; border:0;' colspan='100%'><sup>*</sup> Probability pritings will wait in the queue 87.5%</td>
</tr>
</tfoot>
<tfoot>
<tr>
<td style = 'padding: 0; border:0;' colspan='100%'><sup>*</sup> Probability allInspectors are busy is 46.7%</td>
</tr>
</tfoot>
</table>

## Total cost


```r
std_format <- function(table){
  table %>%   
    kable("html", row.names = TRUE, escape = F) %>%
    kable_styling(bootstrap_options = "striped", full_width = F, position = "left") %>%
    row_spec(nrow(total), bold = T, color = "white", background = "SteelBlue") %>%
    row_spec(nrow(total)-2, bold = T, color = "white", background = "FireBrick") %>%
    column_spec(4, bold = T)
    
}

row_names <- c( "# of Servers",
                    "Average number of posters (WIP)", 
                    "Average number of posters in queue (WIQ)", 
                    "Average wait time (WIP) (in Minutes)",
                    "Average wait time in queue waiting for service (WIQ) (in Minutes)",
                    "Cost of in-process inventory waiting for service (WIQ) ($ Per Hour)",
                    "Cost of all in-process inventory (WIP) ($ Per Hour)",
                    "Cost of servers",
                    "Total Cost ($ Per Hour)")
col_names <- c("Printing Press","Inspection Station")
```



```r
total <- as.data.frame(cbind(unlist(p_sqo$raw_tab),#using 10 presses and 1 inspector
                           unlist(i_sqo$raw_tab))) #using 10 presses and 1 inspector


rownames(total) <- row_names

colnames(total)<- col_names

total$Total = rowSums(total)

options(knitr.kable.NA = '')

std_format(total)
```

<table class="table table-striped" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> Printing Press </th>
   <th style="text-align:right;"> Inspection Station </th>
   <th style="text-align:right;"> Total </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> # of Servers </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;font-weight: bold;"> 11.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Average number of posters (WIP) </td>
   <td style="text-align:right;"> 7.52 </td>
   <td style="text-align:right;"> 7.00 </td>
   <td style="text-align:right;font-weight: bold;"> 14.52 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Average number of posters in queue (WIQ) </td>
   <td style="text-align:right;"> 0.52 </td>
   <td style="text-align:right;"> 6.12 </td>
   <td style="text-align:right;font-weight: bold;"> 6.64 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Average wait time (WIP) (in Minutes) </td>
   <td style="text-align:right;"> 64.43 </td>
   <td style="text-align:right;"> 60.00 </td>
   <td style="text-align:right;font-weight: bold;"> 124.43 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Average wait time in queue waiting for service (WIQ) (in Minutes) </td>
   <td style="text-align:right;"> 4.43 </td>
   <td style="text-align:right;"> 52.50 </td>
   <td style="text-align:right;font-weight: bold;"> 56.93 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cost of in-process inventory waiting for service (WIQ) ($ Per Hour) </td>
   <td style="text-align:right;"> 4.14 </td>
   <td style="text-align:right;"> 49.00 </td>
   <td style="text-align:right;font-weight: bold;"> 53.14 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;color: white !important;background-color: FireBrick !important;"> Cost of all in-process inventory (WIP) ($ Per Hour) </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: FireBrick !important;"> 60.14 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: FireBrick !important;"> 56.00 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: FireBrick !important;font-weight: bold;"> 116.14 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cost of servers </td>
   <td style="text-align:right;"> 49.00 </td>
   <td style="text-align:right;"> 17.00 </td>
   <td style="text-align:right;font-weight: bold;"> 66.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;color: white !important;background-color: SteelBlue !important;"> Total Cost ($ Per Hour) </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: SteelBlue !important;"> 109.14 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: SteelBlue !important;"> 73.00 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: SteelBlue !important;font-weight: bold;"> 182.14 </td>
  </tr>
</tbody>
</table>


With this scenario we have a average total cost of \$182.14 and a average in process cost of \$53.14.  

# Proposal 1: Increase printing time.  

+ Idea: Flow reduction. Print new posters at a rate of 1.2 per hour.  
+ Impact:  
  - Inspectors would be able to keep up with the printers output;
  - Reduction of \$0.50 (from \$7.00 to \$6.50) in the printing power cost.
  

## Printers:   

```r
# Parameters
p_lam_1 <- 7
p_mu_1 <-  1/1.2
p_s_1 <- 10

p_rho_1 <- p_lam_1/(p_mu_1*p_s_1)

# Probability of 0 prints
p_p0_1 <- 1 - p_rho_1
```

+ A printer is expected to be busy 84% of times with this proposal.



```r
# Set up problem with criteria and 7 poster papers
n <- 20


p_prop1 <- queue_model(lambda = p_lam_1,
                     mu = p_mu_1,
                     s = p_s_1,
                     n = n,
                     queue_cost = 8,
                     work_cost = 6.5,
                     use_fixed_cost = FALSE,
                     service_name = "Printers")

p_prop1$html_tab
```

<table class="table table-striped table-bordered" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> # Printers </th>
   <th style="text-align:right;"> L </th>
   <th style="text-align:right;"> Lq </th>
   <th style="text-align:right;"> W (minutes) </th>
   <th style="text-align:right;"> Wq (minutes) </th>
   <th style="text-align:right;"> Queue Cost </th>
   <th style="text-align:right;"> In-proccess Cost </th>
   <th style="text-align:right;"> Service Cost </th>
   <th style="text-align:right;"> Total Cost </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 11.05 </td>
   <td style="text-align:right;"> 2.65 </td>
   <td style="text-align:right;"> 94.69 </td>
   <td style="text-align:right;"> 22.69 </td>
   <td style="text-align:right;"> 21.18 </td>
   <td style="text-align:right;"> 88.38 </td>
   <td style="text-align:right;"> 54.6 </td>
   <td style="text-align:right;"> 142.98 </td>
  </tr>
</tbody>
<tfoot>
<tr>
<td style = 'padding: 0; border:0;' colspan='100%'><sup>*</sup> Probability pritings will wait in the queue 50.4%</td>
</tr>
</tfoot>
<tfoot>
<tr>
<td style = 'padding: 0; border:0;' colspan='100%'><sup>*</sup> Probability allPrinters are busy is 14%</td>
</tr>
</tfoot>
</table>


+ Bellow we can see the original values for comparisson:  

<table class="table table-striped table-bordered" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> # Printers </th>
   <th style="text-align:right;"> L </th>
   <th style="text-align:right;"> Lq </th>
   <th style="text-align:right;"> W (minutes) </th>
   <th style="text-align:right;"> Wq (minutes) </th>
   <th style="text-align:right;"> Queue Cost </th>
   <th style="text-align:right;"> In-proccess Cost </th>
   <th style="text-align:right;"> Service Cost </th>
   <th style="text-align:right;"> Total Cost </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 7.52 </td>
   <td style="text-align:right;"> 0.52 </td>
   <td style="text-align:right;"> 64.43 </td>
   <td style="text-align:right;"> 4.43 </td>
   <td style="text-align:right;"> 4.14 </td>
   <td style="text-align:right;"> 60.14 </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:right;"> 109.14 </td>
  </tr>
</tbody>
<tfoot>
<tr>
<td style = 'padding: 0; border:0;' colspan='100%'><sup>*</sup> Probability pritings will wait in the queue 22.2%</td>
</tr>
</tfoot>
<tfoot>
<tr>
<td style = 'padding: 0; border:0;' colspan='100%'><sup>*</sup> Probability allPrinters are busy is 7.9%</td>
</tr>
</tfoot>
</table>


## Inspection:   

```r
# Parameters
i_lam_1 <- 7
i_mu_1 <-  8
i_s_1 <- 1

i_rho_1 <- i_lam_1/(i_mu_1*i_s_1)

# Probability of 0 prints
i_p0_1 <- 1 - i_rho_1
```

+ The inspector is expected to be busy 87.5% of times with this proposal.



```r
# Set up problem with criteria and 20 inspections
n <- 20

i_prop1 <- queue_model(lambda = i_lam_1,
                     mu = i_mu_1,
                     s = i_s_1,
                     n = n,
                     queue_cost = 8,
                     work_cost = 17,
                     use_fixed_cost = TRUE,
                     service_name = "Inspectors")

prop1_total_cost <- i_prop1$raw_tab$`Total Cost` + p_prop1$raw_tab$`Total Cost`
prop1_queue_cost <- i_prop1$raw_tab$`Queue Cost` + p_prop1$raw_tab$`Queue Cost`
```


## Total cost


```r
prop_1 <- as.data.frame(cbind(unlist(p_prop1$raw_tab),
                              unlist(i_prop1$raw_tab)
                              ))

rownames(prop_1) <- row_names

colnames(prop_1)<- col_names

prop_1$Total = rowSums(prop_1)

total_1 <- cbind(total, prop_1)

options(knitr.kable.NA = '')

std_format(total_1) %>% 
  column_spec(7, bold = T) %>% 
  add_header_above(c(" ", "Status Quo" = 3, "Proposal 1: Decrease printing rate" = 3))%>% 
  cat(., file = "../Final_Project/Tables/prop_1.html")
htmltools::includeHTML("../Final_Project/Tables/prop_1.html")
```

<!--html_preserve--><table class="table table-striped" style="width: auto !important; ">
 <thead>
<tr>
<th style="border-bottom:hidden" colspan="1"></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="3"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Status Quo</div></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="3"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Proposal 1: Decrease printing rate</div></th>
</tr>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> Printing Press </th>
   <th style="text-align:right;"> Inspection Station </th>
   <th style="text-align:right;"> Total </th>
   <th style="text-align:right;"> Printing Press </th>
   <th style="text-align:right;"> Inspection Station </th>
   <th style="text-align:right;"> Total </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> # of Servers </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;font-weight: bold;"> 11.00 </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;font-weight: bold;"> 11.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Average number of posters (WIP) </td>
   <td style="text-align:right;"> 7.52 </td>
   <td style="text-align:right;"> 7.00 </td>
   <td style="text-align:right;font-weight: bold;"> 14.52 </td>
   <td style="text-align:right;"> 11.05 </td>
   <td style="text-align:right;"> 7.00 </td>
   <td style="text-align:right;font-weight: bold;"> 18.05 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Average number of posters in queue (WIQ) </td>
   <td style="text-align:right;"> 0.52 </td>
   <td style="text-align:right;"> 6.12 </td>
   <td style="text-align:right;font-weight: bold;"> 6.64 </td>
   <td style="text-align:right;"> 2.65 </td>
   <td style="text-align:right;"> 6.12 </td>
   <td style="text-align:right;font-weight: bold;"> 8.77 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Average wait time (WIP) (in Minutes) </td>
   <td style="text-align:right;"> 64.43 </td>
   <td style="text-align:right;"> 60.00 </td>
   <td style="text-align:right;font-weight: bold;"> 124.43 </td>
   <td style="text-align:right;"> 94.69 </td>
   <td style="text-align:right;"> 60.00 </td>
   <td style="text-align:right;font-weight: bold;"> 154.69 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Average wait time in queue waiting for service (WIQ) (in Minutes) </td>
   <td style="text-align:right;"> 4.43 </td>
   <td style="text-align:right;"> 52.50 </td>
   <td style="text-align:right;font-weight: bold;"> 56.93 </td>
   <td style="text-align:right;"> 22.69 </td>
   <td style="text-align:right;"> 52.50 </td>
   <td style="text-align:right;font-weight: bold;"> 75.19 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cost of in-process inventory waiting for service (WIQ) ($ Per Hour) </td>
   <td style="text-align:right;"> 4.14 </td>
   <td style="text-align:right;"> 49.00 </td>
   <td style="text-align:right;font-weight: bold;"> 53.14 </td>
   <td style="text-align:right;"> 21.18 </td>
   <td style="text-align:right;"> 49.00 </td>
   <td style="text-align:right;font-weight: bold;"> 70.18 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;color: white !important;background-color: FireBrick !important;"> Cost of all in-process inventory (WIP) ($ Per Hour) </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: FireBrick !important;"> 60.14 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: FireBrick !important;"> 56.00 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: FireBrick !important;font-weight: bold;"> 116.14 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: FireBrick !important;"> 88.38 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: FireBrick !important;"> 56.00 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: FireBrick !important;font-weight: bold;"> 144.38 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cost of servers </td>
   <td style="text-align:right;"> 49.00 </td>
   <td style="text-align:right;"> 17.00 </td>
   <td style="text-align:right;font-weight: bold;"> 66.00 </td>
   <td style="text-align:right;"> 54.60 </td>
   <td style="text-align:right;"> 17.00 </td>
   <td style="text-align:right;font-weight: bold;"> 71.60 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;color: white !important;background-color: SteelBlue !important;"> Total Cost ($ Per Hour) </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: SteelBlue !important;"> 109.14 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: SteelBlue !important;"> 73.00 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: SteelBlue !important;font-weight: bold;"> 182.14 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: SteelBlue !important;"> 142.98 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: SteelBlue !important;"> 73.00 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: SteelBlue !important;font-weight: bold;"> 215.98 </td>
  </tr>
</tbody>
</table><!--/html_preserve-->

  
# Proposal 1b: Decrease printing time and number of printers.  

+ Idea: Speed up the printing process.  
+ Impact:  
  - Inspectors would be able to keep up with the printers output;
  - Increase of \$0.50 (from \$7.00 to \$6.50) in the printing power cost.
  - Decrease number of printers from 10 to 8
  

## Printers:   

```r
# Parameters
p_lam_1b <- 7
p_mu_1b <-  1.2
p_s_1b <- 8

p_rho_1b <- p_lam_1b/(p_mu_1b*p_s_1b)

# Probability of 0 prints
p_p0_1b <- 1 - p_rho_1b
```

+ A printer is expected to be busy 84% of times with this proposal.



```r
# Set up problem with criteria and 7 poster papers
n <- 20


p_prop1b <- queue_model(lambda = p_lam_1b,
                     mu = p_mu_1b,
                     s = p_s_1b,
                     n = n,
                     queue_cost = 8,
                     work_cost = 7.5,
                     use_fixed_cost = FALSE,
                     service_name = "Printers")

p_prop1b$html_tab
```

<table class="table table-striped table-bordered" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> # Printers </th>
   <th style="text-align:right;"> L </th>
   <th style="text-align:right;"> Lq </th>
   <th style="text-align:right;"> W (minutes) </th>
   <th style="text-align:right;"> Wq (minutes) </th>
   <th style="text-align:right;"> Queue Cost </th>
   <th style="text-align:right;"> In-proccess Cost </th>
   <th style="text-align:right;"> Service Cost </th>
   <th style="text-align:right;"> Total Cost </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 6.69 </td>
   <td style="text-align:right;"> 0.86 </td>
   <td style="text-align:right;"> 57.37 </td>
   <td style="text-align:right;"> 7.37 </td>
   <td style="text-align:right;"> 6.88 </td>
   <td style="text-align:right;"> 53.54 </td>
   <td style="text-align:right;"> 43.75 </td>
   <td style="text-align:right;"> 97.29 </td>
  </tr>
</tbody>
<tfoot>
<tr>
<td style = 'padding: 0; border:0;' colspan='100%'><sup>*</sup> Probability pritings will wait in the queue 31.9%</td>
</tr>
</tfoot>
<tfoot>
<tr>
<td style = 'padding: 0; border:0;' colspan='100%'><sup>*</sup> Probability allPrinters are busy is 11.3%</td>
</tr>
</tfoot>
</table>


+ Bellow we can see the original values for comparisson:  

<table class="table table-striped table-bordered" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> # Printers </th>
   <th style="text-align:right;"> L </th>
   <th style="text-align:right;"> Lq </th>
   <th style="text-align:right;"> W (minutes) </th>
   <th style="text-align:right;"> Wq (minutes) </th>
   <th style="text-align:right;"> Queue Cost </th>
   <th style="text-align:right;"> In-proccess Cost </th>
   <th style="text-align:right;"> Service Cost </th>
   <th style="text-align:right;"> Total Cost </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 7.52 </td>
   <td style="text-align:right;"> 0.52 </td>
   <td style="text-align:right;"> 64.43 </td>
   <td style="text-align:right;"> 4.43 </td>
   <td style="text-align:right;"> 4.14 </td>
   <td style="text-align:right;"> 60.14 </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:right;"> 109.14 </td>
  </tr>
</tbody>
<tfoot>
<tr>
<td style = 'padding: 0; border:0;' colspan='100%'><sup>*</sup> Probability pritings will wait in the queue 22.2%</td>
</tr>
</tfoot>
<tfoot>
<tr>
<td style = 'padding: 0; border:0;' colspan='100%'><sup>*</sup> Probability allPrinters are busy is 7.9%</td>
</tr>
</tfoot>
</table>

+ Bellow we can see the original proposal 1 for comparisson:  

<table class="table table-striped table-bordered" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> # Printers </th>
   <th style="text-align:right;"> L </th>
   <th style="text-align:right;"> Lq </th>
   <th style="text-align:right;"> W (minutes) </th>
   <th style="text-align:right;"> Wq (minutes) </th>
   <th style="text-align:right;"> Queue Cost </th>
   <th style="text-align:right;"> In-proccess Cost </th>
   <th style="text-align:right;"> Service Cost </th>
   <th style="text-align:right;"> Total Cost </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 11.05 </td>
   <td style="text-align:right;"> 2.65 </td>
   <td style="text-align:right;"> 94.69 </td>
   <td style="text-align:right;"> 22.69 </td>
   <td style="text-align:right;"> 21.18 </td>
   <td style="text-align:right;"> 88.38 </td>
   <td style="text-align:right;"> 54.6 </td>
   <td style="text-align:right;"> 142.98 </td>
  </tr>
</tbody>
<tfoot>
<tr>
<td style = 'padding: 0; border:0;' colspan='100%'><sup>*</sup> Probability pritings will wait in the queue 50.4%</td>
</tr>
</tfoot>
<tfoot>
<tr>
<td style = 'padding: 0; border:0;' colspan='100%'><sup>*</sup> Probability allPrinters are busy is 14%</td>
</tr>
</tfoot>
</table>



```r
prop1b_total_cost <- i_prop1$raw_tab$`Total Cost` + p_prop1b$raw_tab$`Total Cost`
prop1b_queue_cost <- i_prop1$raw_tab$`Queue Cost` + p_prop1b$raw_tab$`Queue Cost`
```

## Total cost


```r
prop_1b <- as.data.frame(cbind(unlist(p_prop1b$raw_tab),
                               unlist(i_prop1$raw_tab)
                               ))


rownames(prop_1b) <- row_names

colnames(prop_1b)<- col_names

prop_1b$Total = rowSums(prop_1b)

total_1b <- cbind(total, prop_1b)

options(knitr.kable.NA = '')

std_format(total_1b) %>% 
  column_spec(7, bold = T) %>% 
  add_header_above(c(" ", "Status Quo" = 3, "Proposal 1b: Increase printing rate" = 3))%>% 
  cat(., file = "../Final_Project/Tables/prop_1b.html")
htmltools::includeHTML("../Final_Project/Tables/prop_1b.html")
```

<!--html_preserve--><table class="table table-striped" style="width: auto !important; ">
 <thead>
<tr>
<th style="border-bottom:hidden" colspan="1"></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="3"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Status Quo</div></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="3"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Proposal 1b: Increase printing rate</div></th>
</tr>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> Printing Press </th>
   <th style="text-align:right;"> Inspection Station </th>
   <th style="text-align:right;"> Total </th>
   <th style="text-align:right;"> Printing Press </th>
   <th style="text-align:right;"> Inspection Station </th>
   <th style="text-align:right;"> Total </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> # of Servers </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;font-weight: bold;"> 11.00 </td>
   <td style="text-align:right;"> 8.00 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;font-weight: bold;"> 9.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Average number of posters (WIP) </td>
   <td style="text-align:right;"> 7.52 </td>
   <td style="text-align:right;"> 7.00 </td>
   <td style="text-align:right;font-weight: bold;"> 14.52 </td>
   <td style="text-align:right;"> 6.69 </td>
   <td style="text-align:right;"> 7.00 </td>
   <td style="text-align:right;font-weight: bold;"> 13.69 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Average number of posters in queue (WIQ) </td>
   <td style="text-align:right;"> 0.52 </td>
   <td style="text-align:right;"> 6.12 </td>
   <td style="text-align:right;font-weight: bold;"> 6.64 </td>
   <td style="text-align:right;"> 0.86 </td>
   <td style="text-align:right;"> 6.12 </td>
   <td style="text-align:right;font-weight: bold;"> 6.98 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Average wait time (WIP) (in Minutes) </td>
   <td style="text-align:right;"> 64.43 </td>
   <td style="text-align:right;"> 60.00 </td>
   <td style="text-align:right;font-weight: bold;"> 124.43 </td>
   <td style="text-align:right;"> 57.37 </td>
   <td style="text-align:right;"> 60.00 </td>
   <td style="text-align:right;font-weight: bold;"> 117.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Average wait time in queue waiting for service (WIQ) (in Minutes) </td>
   <td style="text-align:right;"> 4.43 </td>
   <td style="text-align:right;"> 52.50 </td>
   <td style="text-align:right;font-weight: bold;"> 56.93 </td>
   <td style="text-align:right;"> 7.37 </td>
   <td style="text-align:right;"> 52.50 </td>
   <td style="text-align:right;font-weight: bold;"> 59.87 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cost of in-process inventory waiting for service (WIQ) ($ Per Hour) </td>
   <td style="text-align:right;"> 4.14 </td>
   <td style="text-align:right;"> 49.00 </td>
   <td style="text-align:right;font-weight: bold;"> 53.14 </td>
   <td style="text-align:right;"> 6.88 </td>
   <td style="text-align:right;"> 49.00 </td>
   <td style="text-align:right;font-weight: bold;"> 55.88 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;color: white !important;background-color: FireBrick !important;"> Cost of all in-process inventory (WIP) ($ Per Hour) </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: FireBrick !important;"> 60.14 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: FireBrick !important;"> 56.00 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: FireBrick !important;font-weight: bold;"> 116.14 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: FireBrick !important;"> 53.54 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: FireBrick !important;"> 56.00 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: FireBrick !important;font-weight: bold;"> 109.54 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cost of servers </td>
   <td style="text-align:right;"> 49.00 </td>
   <td style="text-align:right;"> 17.00 </td>
   <td style="text-align:right;font-weight: bold;"> 66.00 </td>
   <td style="text-align:right;"> 43.75 </td>
   <td style="text-align:right;"> 17.00 </td>
   <td style="text-align:right;font-weight: bold;"> 60.75 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;color: white !important;background-color: SteelBlue !important;"> Total Cost ($ Per Hour) </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: SteelBlue !important;"> 109.14 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: SteelBlue !important;"> 73.00 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: SteelBlue !important;font-weight: bold;"> 182.14 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: SteelBlue !important;"> 97.29 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: SteelBlue !important;"> 73.00 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: SteelBlue !important;font-weight: bold;"> 170.29 </td>
  </tr>
</tbody>
</table><!--/html_preserve-->

With this scenario we have a average total cost of \$170.29 and a average in process cost of \$55.88. 


# Proposal 2: Substitute a more experienced inspector for this task.  

+ Idea: Increase flow on the inspection. 
+ Impact:  
  - Increases inspection rate from 8 per hour to 8.57 per hour
  - Increases compensation in \$2.00 (from \$17.00 to \$19.00).

## Inspection:   

```r
# Parameters
i_lam_2 <- 7
i_mu_2 <-  60/7
i_s_2 <- 1

i_rho_2 <- i_lam_2/(i_mu_2*i_s_2)

# Probability of 0 prints
i_p0_2 <- 1 - i_rho_2
```

+ The inspector is expected to be busy 81.67% of times with this proposal.



```r
# Set up problem with criteria and 20 inspections
n <- 20

i_prop2 <- queue_model(lambda = i_lam_2,
                     mu = i_mu_2,
                     s = i_s_2,
                     n = n,
                     queue_cost = 8,
                     work_cost = 19,
                     use_fixed_cost = TRUE,
                     service_name = "Inspectors")

prop2_total_cost <- i_prop2$raw_tab$`Total Cost` + p_sqo$raw_tab$`Total Cost`
prop2_queue_cost <- i_prop2$raw_tab$`Queue Cost` + p_sqo$raw_tab$`Queue Cost`
i_prop2$html_tab
```

<table class="table table-striped table-bordered" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> # Inspectors </th>
   <th style="text-align:right;"> L </th>
   <th style="text-align:right;"> Lq </th>
   <th style="text-align:right;"> W (minutes) </th>
   <th style="text-align:right;"> Wq (minutes) </th>
   <th style="text-align:right;"> Queue Cost </th>
   <th style="text-align:right;"> In-proccess Cost </th>
   <th style="text-align:right;"> Service Cost </th>
   <th style="text-align:right;"> Total Cost </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 4.45 </td>
   <td style="text-align:right;"> 3.64 </td>
   <td style="text-align:right;"> 38.18 </td>
   <td style="text-align:right;"> 31.18 </td>
   <td style="text-align:right;"> 29.1 </td>
   <td style="text-align:right;"> 35.64 </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 54.64 </td>
  </tr>
</tbody>
<tfoot>
<tr>
<td style = 'padding: 0; border:0;' colspan='100%'><sup>*</sup> Probability pritings will wait in the queue 81.7%</td>
</tr>
</tfoot>
<tfoot>
<tr>
<td style = 'padding: 0; border:0;' colspan='100%'><sup>*</sup> Probability allInspectors are busy is 45%</td>
</tr>
</tfoot>
</table>

+ Bellow we can see the original values for comparisson:  

<table class="table table-striped table-bordered" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> # Inspectors </th>
   <th style="text-align:right;"> L </th>
   <th style="text-align:right;"> Lq </th>
   <th style="text-align:right;"> W (minutes) </th>
   <th style="text-align:right;"> Wq (minutes) </th>
   <th style="text-align:right;"> Queue Cost </th>
   <th style="text-align:right;"> In-proccess Cost </th>
   <th style="text-align:right;"> Service Cost </th>
   <th style="text-align:right;"> Total Cost </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 6.12 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:right;"> 52.5 </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:right;"> 56 </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 73 </td>
  </tr>
</tbody>
<tfoot>
<tr>
<td style = 'padding: 0; border:0;' colspan='100%'><sup>*</sup> Probability pritings will wait in the queue 87.5%</td>
</tr>
</tfoot>
<tfoot>
<tr>
<td style = 'padding: 0; border:0;' colspan='100%'><sup>*</sup> Probability allInspectors are busy is 46.7%</td>
</tr>
</tfoot>
</table>

## Total cost


```r
prop_2 <- as.data.frame(cbind(unlist(p_sqo$raw_tab),
                           unlist(i_prop2$raw_tab))) 

rownames(prop_2) <- row_names

colnames(prop_2)<- col_names

prop_2$Total = rowSums(prop_2)

total_2 <- cbind(total, prop_2)

options(knitr.kable.NA = '')

std_format(total_2) %>% 
  column_spec(7, bold = T) %>%  
  add_header_above(c(" ", "Status Quo" = 3, "Proposal 2: Senior Inspector" = 3))%>% 
  cat(., file = "../Final_Project/Tables/prop_2.html")
htmltools::includeHTML("../Final_Project/Tables/prop_2.html")
```

<!--html_preserve--><table class="table table-striped" style="width: auto !important; ">
 <thead>
<tr>
<th style="border-bottom:hidden" colspan="1"></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="3"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Status Quo</div></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="3"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Proposal 2: Senior Inspector</div></th>
</tr>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> Printing Press </th>
   <th style="text-align:right;"> Inspection Station </th>
   <th style="text-align:right;"> Total </th>
   <th style="text-align:right;"> Printing Press </th>
   <th style="text-align:right;"> Inspection Station </th>
   <th style="text-align:right;"> Total </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> # of Servers </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;font-weight: bold;"> 11.00 </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;font-weight: bold;"> 11.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Average number of posters (WIP) </td>
   <td style="text-align:right;"> 7.52 </td>
   <td style="text-align:right;"> 7.00 </td>
   <td style="text-align:right;font-weight: bold;"> 14.52 </td>
   <td style="text-align:right;"> 7.52 </td>
   <td style="text-align:right;"> 4.45 </td>
   <td style="text-align:right;font-weight: bold;"> 11.97 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Average number of posters in queue (WIQ) </td>
   <td style="text-align:right;"> 0.52 </td>
   <td style="text-align:right;"> 6.12 </td>
   <td style="text-align:right;font-weight: bold;"> 6.64 </td>
   <td style="text-align:right;"> 0.52 </td>
   <td style="text-align:right;"> 3.64 </td>
   <td style="text-align:right;font-weight: bold;"> 4.16 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Average wait time (WIP) (in Minutes) </td>
   <td style="text-align:right;"> 64.43 </td>
   <td style="text-align:right;"> 60.00 </td>
   <td style="text-align:right;font-weight: bold;"> 124.43 </td>
   <td style="text-align:right;"> 64.43 </td>
   <td style="text-align:right;"> 38.18 </td>
   <td style="text-align:right;font-weight: bold;"> 102.61 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Average wait time in queue waiting for service (WIQ) (in Minutes) </td>
   <td style="text-align:right;"> 4.43 </td>
   <td style="text-align:right;"> 52.50 </td>
   <td style="text-align:right;font-weight: bold;"> 56.93 </td>
   <td style="text-align:right;"> 4.43 </td>
   <td style="text-align:right;"> 31.18 </td>
   <td style="text-align:right;font-weight: bold;"> 35.61 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cost of in-process inventory waiting for service (WIQ) ($ Per Hour) </td>
   <td style="text-align:right;"> 4.14 </td>
   <td style="text-align:right;"> 49.00 </td>
   <td style="text-align:right;font-weight: bold;"> 53.14 </td>
   <td style="text-align:right;"> 4.14 </td>
   <td style="text-align:right;"> 29.10 </td>
   <td style="text-align:right;font-weight: bold;"> 33.24 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;color: white !important;background-color: FireBrick !important;"> Cost of all in-process inventory (WIP) ($ Per Hour) </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: FireBrick !important;"> 60.14 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: FireBrick !important;"> 56.00 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: FireBrick !important;font-weight: bold;"> 116.14 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: FireBrick !important;"> 60.14 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: FireBrick !important;"> 35.64 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: FireBrick !important;font-weight: bold;"> 95.78 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cost of servers </td>
   <td style="text-align:right;"> 49.00 </td>
   <td style="text-align:right;"> 17.00 </td>
   <td style="text-align:right;font-weight: bold;"> 66.00 </td>
   <td style="text-align:right;"> 49.00 </td>
   <td style="text-align:right;"> 19.00 </td>
   <td style="text-align:right;font-weight: bold;"> 68.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;color: white !important;background-color: SteelBlue !important;"> Total Cost ($ Per Hour) </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: SteelBlue !important;"> 109.14 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: SteelBlue !important;"> 73.00 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: SteelBlue !important;font-weight: bold;"> 182.14 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: SteelBlue !important;"> 109.14 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: SteelBlue !important;"> 54.64 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: SteelBlue !important;font-weight: bold;"> 163.78 </td>
  </tr>
</tbody>
</table><!--/html_preserve-->


With this scenario we have a average total cost of \$163.78 and a average in process cost of \$33.24. 

# Proposal 2b: Substitute a more experienced inspector for this task and increase the number of inspectors.  

+ Idea: Increase flow on the inspection. 
+ Impact:  
  - Increases inspection rate from 8 per hour to 8.57 per hour
  - Increases compensation in \$2.00 (from \$17.00 to \$19.00).
  - Increase to 2 senior inspectors

## Inspection:   

```r
# Parameters
i_lam_2b <- 7
i_mu_2b <-  60/7
i_s_2b <- 2

i_rho_2b <- i_lam_2b/(i_mu_2b*i_s_2b)

# Probability of 0 prints
i_p0_2b <- 1 - i_rho_2b
```

+ The inspectors are expected to be busy 81.67% of times with this proposal.



```r
# Set up problem with criteria and 20 inspections
n <- 20

i_prop2b <- queue_model(lambda = i_lam_2b,
                     mu = i_mu_2b,
                     s = i_s_2b,
                     n = n,
                     queue_cost = 8,
                     work_cost = 19,
                     use_fixed_cost = TRUE,
                     service_name = "Inspectors")
i_prop2$html_tab
```

<table class="table table-striped table-bordered" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> # Inspectors </th>
   <th style="text-align:right;"> L </th>
   <th style="text-align:right;"> Lq </th>
   <th style="text-align:right;"> W (minutes) </th>
   <th style="text-align:right;"> Wq (minutes) </th>
   <th style="text-align:right;"> Queue Cost </th>
   <th style="text-align:right;"> In-proccess Cost </th>
   <th style="text-align:right;"> Service Cost </th>
   <th style="text-align:right;"> Total Cost </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 4.45 </td>
   <td style="text-align:right;"> 3.64 </td>
   <td style="text-align:right;"> 38.18 </td>
   <td style="text-align:right;"> 31.18 </td>
   <td style="text-align:right;"> 29.1 </td>
   <td style="text-align:right;"> 35.64 </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 54.64 </td>
  </tr>
</tbody>
<tfoot>
<tr>
<td style = 'padding: 0; border:0;' colspan='100%'><sup>*</sup> Probability pritings will wait in the queue 81.7%</td>
</tr>
</tfoot>
<tfoot>
<tr>
<td style = 'padding: 0; border:0;' colspan='100%'><sup>*</sup> Probability allInspectors are busy is 45%</td>
</tr>
</tfoot>
</table>
## Total cost


```r
prop_2b <- as.data.frame(cbind(unlist(p_sqo$raw_tab),
                               unlist(i_prop2b$raw_tab)
                               )) 

rownames(prop_2b) <- row_names

colnames(prop_2b)<- col_names

prop_2b$Total = rowSums(prop_2b)

total_2b <- cbind(total, prop_2b)

options(knitr.kable.NA = '')

std_format(total_2b) %>% 
  column_spec(7, bold = T) %>% 
  add_header_above(c(" ", "Status Quo" = 3, "Proposal 2b: 2 Senior Inspectors" = 3))%>% 
  cat(., file = "../Final_Project/Tables/prop_2b.html")
htmltools::includeHTML("../Final_Project/Tables/prop_2b.html")
```

<!--html_preserve--><table class="table table-striped" style="width: auto !important; ">
 <thead>
<tr>
<th style="border-bottom:hidden" colspan="1"></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="3"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Status Quo</div></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="3"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Proposal 2b: 2 Senior Inspectors</div></th>
</tr>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> Printing Press </th>
   <th style="text-align:right;"> Inspection Station </th>
   <th style="text-align:right;"> Total </th>
   <th style="text-align:right;"> Printing Press </th>
   <th style="text-align:right;"> Inspection Station </th>
   <th style="text-align:right;"> Total </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> # of Servers </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;font-weight: bold;"> 11.00 </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:right;"> 2.00 </td>
   <td style="text-align:right;font-weight: bold;"> 12.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Average number of posters (WIP) </td>
   <td style="text-align:right;"> 7.52 </td>
   <td style="text-align:right;"> 7.00 </td>
   <td style="text-align:right;font-weight: bold;"> 14.52 </td>
   <td style="text-align:right;"> 7.52 </td>
   <td style="text-align:right;"> 0.98 </td>
   <td style="text-align:right;font-weight: bold;"> 8.50 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Average number of posters in queue (WIQ) </td>
   <td style="text-align:right;"> 0.52 </td>
   <td style="text-align:right;"> 6.12 </td>
   <td style="text-align:right;font-weight: bold;"> 6.64 </td>
   <td style="text-align:right;"> 0.52 </td>
   <td style="text-align:right;"> 0.16 </td>
   <td style="text-align:right;font-weight: bold;"> 0.68 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Average wait time (WIP) (in Minutes) </td>
   <td style="text-align:right;"> 64.43 </td>
   <td style="text-align:right;"> 60.00 </td>
   <td style="text-align:right;font-weight: bold;"> 124.43 </td>
   <td style="text-align:right;"> 64.43 </td>
   <td style="text-align:right;"> 8.40 </td>
   <td style="text-align:right;font-weight: bold;"> 72.83 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Average wait time in queue waiting for service (WIQ) (in Minutes) </td>
   <td style="text-align:right;"> 4.43 </td>
   <td style="text-align:right;"> 52.50 </td>
   <td style="text-align:right;font-weight: bold;"> 56.93 </td>
   <td style="text-align:right;"> 4.43 </td>
   <td style="text-align:right;"> 1.40 </td>
   <td style="text-align:right;font-weight: bold;"> 5.83 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cost of in-process inventory waiting for service (WIQ) ($ Per Hour) </td>
   <td style="text-align:right;"> 4.14 </td>
   <td style="text-align:right;"> 49.00 </td>
   <td style="text-align:right;font-weight: bold;"> 53.14 </td>
   <td style="text-align:right;"> 4.14 </td>
   <td style="text-align:right;"> 1.31 </td>
   <td style="text-align:right;font-weight: bold;"> 5.45 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;color: white !important;background-color: FireBrick !important;"> Cost of all in-process inventory (WIP) ($ Per Hour) </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: FireBrick !important;"> 60.14 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: FireBrick !important;"> 56.00 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: FireBrick !important;font-weight: bold;"> 116.14 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: FireBrick !important;"> 60.14 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: FireBrick !important;"> 7.84 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: FireBrick !important;font-weight: bold;"> 67.98 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cost of servers </td>
   <td style="text-align:right;"> 49.00 </td>
   <td style="text-align:right;"> 17.00 </td>
   <td style="text-align:right;font-weight: bold;"> 66.00 </td>
   <td style="text-align:right;"> 49.00 </td>
   <td style="text-align:right;"> 38.00 </td>
   <td style="text-align:right;font-weight: bold;"> 87.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;color: white !important;background-color: SteelBlue !important;"> Total Cost ($ Per Hour) </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: SteelBlue !important;"> 109.14 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: SteelBlue !important;"> 73.00 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: SteelBlue !important;font-weight: bold;"> 182.14 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: SteelBlue !important;"> 109.14 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: SteelBlue !important;"> 45.84 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: SteelBlue !important;font-weight: bold;"> 154.98 </td>
  </tr>
</tbody>
</table><!--/html_preserve-->



# Proposal 3: Join proposal 1B and 2B



```r
prop_3 <- as.data.frame(cbind(unlist(p_prop1b$raw_tab),
                              unlist(i_prop2b$raw_tab)
                              )) 

rownames(prop_3) <- row_names

colnames(prop_3)<- col_names

prop_3$Total = rowSums(prop_3)

total_3 <- cbind(total, prop_3)

options(knitr.kable.NA = '')

std_format(total_3) %>% 
  column_spec(7, bold = T) %>% 
  add_header_above(c(" ", "Status Quo" = 3, "Proposal 1b + Proposal 2b" = 3))%>% 
  cat(., file = "../Final_Project/Tables/prop_3.html")
htmltools::includeHTML("../Final_Project/Tables/prop_3.html")
```

<!--html_preserve--><table class="table table-striped" style="width: auto !important; ">
 <thead>
<tr>
<th style="border-bottom:hidden" colspan="1"></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="3"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Status Quo</div></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="3"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Proposal 1b + Proposal 2b</div></th>
</tr>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> Printing Press </th>
   <th style="text-align:right;"> Inspection Station </th>
   <th style="text-align:right;"> Total </th>
   <th style="text-align:right;"> Printing Press </th>
   <th style="text-align:right;"> Inspection Station </th>
   <th style="text-align:right;"> Total </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> # of Servers </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;font-weight: bold;"> 11.00 </td>
   <td style="text-align:right;"> 8.00 </td>
   <td style="text-align:right;"> 2.00 </td>
   <td style="text-align:right;font-weight: bold;"> 10.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Average number of posters (WIP) </td>
   <td style="text-align:right;"> 7.52 </td>
   <td style="text-align:right;"> 7.00 </td>
   <td style="text-align:right;font-weight: bold;"> 14.52 </td>
   <td style="text-align:right;"> 6.69 </td>
   <td style="text-align:right;"> 0.98 </td>
   <td style="text-align:right;font-weight: bold;"> 7.67 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Average number of posters in queue (WIQ) </td>
   <td style="text-align:right;"> 0.52 </td>
   <td style="text-align:right;"> 6.12 </td>
   <td style="text-align:right;font-weight: bold;"> 6.64 </td>
   <td style="text-align:right;"> 0.86 </td>
   <td style="text-align:right;"> 0.16 </td>
   <td style="text-align:right;font-weight: bold;"> 1.02 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Average wait time (WIP) (in Minutes) </td>
   <td style="text-align:right;"> 64.43 </td>
   <td style="text-align:right;"> 60.00 </td>
   <td style="text-align:right;font-weight: bold;"> 124.43 </td>
   <td style="text-align:right;"> 57.37 </td>
   <td style="text-align:right;"> 8.40 </td>
   <td style="text-align:right;font-weight: bold;"> 65.77 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Average wait time in queue waiting for service (WIQ) (in Minutes) </td>
   <td style="text-align:right;"> 4.43 </td>
   <td style="text-align:right;"> 52.50 </td>
   <td style="text-align:right;font-weight: bold;"> 56.93 </td>
   <td style="text-align:right;"> 7.37 </td>
   <td style="text-align:right;"> 1.40 </td>
   <td style="text-align:right;font-weight: bold;"> 8.77 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cost of in-process inventory waiting for service (WIQ) ($ Per Hour) </td>
   <td style="text-align:right;"> 4.14 </td>
   <td style="text-align:right;"> 49.00 </td>
   <td style="text-align:right;font-weight: bold;"> 53.14 </td>
   <td style="text-align:right;"> 6.88 </td>
   <td style="text-align:right;"> 1.31 </td>
   <td style="text-align:right;font-weight: bold;"> 8.19 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;color: white !important;background-color: FireBrick !important;"> Cost of all in-process inventory (WIP) ($ Per Hour) </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: FireBrick !important;"> 60.14 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: FireBrick !important;"> 56.00 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: FireBrick !important;font-weight: bold;"> 116.14 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: FireBrick !important;"> 53.54 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: FireBrick !important;"> 7.84 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: FireBrick !important;font-weight: bold;"> 61.38 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cost of servers </td>
   <td style="text-align:right;"> 49.00 </td>
   <td style="text-align:right;"> 17.00 </td>
   <td style="text-align:right;font-weight: bold;"> 66.00 </td>
   <td style="text-align:right;"> 43.75 </td>
   <td style="text-align:right;"> 38.00 </td>
   <td style="text-align:right;font-weight: bold;"> 81.75 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;color: white !important;background-color: SteelBlue !important;"> Total Cost ($ Per Hour) </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: SteelBlue !important;"> 109.14 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: SteelBlue !important;"> 73.00 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: SteelBlue !important;font-weight: bold;"> 182.14 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: SteelBlue !important;"> 97.29 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: SteelBlue !important;"> 45.84 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: SteelBlue !important;font-weight: bold;"> 143.13 </td>
  </tr>
</tbody>
</table><!--/html_preserve-->


# Summary


```r
comp <- as.data.frame(cbind(total$Total,
                            prop_1$Total,
                            prop_1b$Total,
                            prop_2$Total,
                            prop_2b$Total,
                            prop_3$Total)) # variable to compare all proposals 
                      
options(knitr.kable.NA = '')

rownames(comp) <- row_names

colnames(comp)<- c("Status Quo", 
                     "Prop 1",
                     "Prop 1b",
                     "Prop 2",
                     "Prop 2b",
                     "Prop 3")
comp %>%   
  kable("html", row.names = TRUE, escape = F) %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "left") %>%
  row_spec(nrow(total), bold = T, color = "white", background = "SteelBlue") %>%
  row_spec(nrow(total)-2, bold = T, color = "white", background = "FireBrick") %>%
  column_spec(1, width = "120em") %>% 
  column_spec(2, bold = T, width = "15em") %>% 
  column_spec(3:6, width = "15em") %>%
  column_spec(7, bold = T, width = "15em") %>% 
  footnote(general = c("- Proposal 1: Decrease printing rate",
                       "- Proposal 1b: Increase printing rate",
                       "- Proposal 2: Senior Inspector",
                       "- Proposal 2b: 2 Senior Inspectors",
                       "- Proposal 3: Increase printing rate and 2 Senior Inspectors"
                       ) ) %>% 
  cat(., file = "../Final_Project/Tables/compare_prop.html")

htmltools::includeHTML("../Final_Project/Tables/compare_prop.html")
```

<!--html_preserve--><table class="table table-striped" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> Status Quo </th>
   <th style="text-align:right;"> Prop 1 </th>
   <th style="text-align:right;"> Prop 1b </th>
   <th style="text-align:right;"> Prop 2 </th>
   <th style="text-align:right;"> Prop 2b </th>
   <th style="text-align:right;"> Prop 3 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;width: 120em; "> # of Servers </td>
   <td style="text-align:right;width: 15em; font-weight: bold;"> 11.00 </td>
   <td style="text-align:right;width: 15em; "> 11.00 </td>
   <td style="text-align:right;width: 15em; "> 9.00 </td>
   <td style="text-align:right;width: 15em; "> 11.00 </td>
   <td style="text-align:right;width: 15em; "> 12.00 </td>
   <td style="text-align:right;width: 15em; font-weight: bold;"> 10.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 120em; "> Average number of posters (WIP) </td>
   <td style="text-align:right;width: 15em; font-weight: bold;"> 14.52 </td>
   <td style="text-align:right;width: 15em; "> 18.05 </td>
   <td style="text-align:right;width: 15em; "> 13.69 </td>
   <td style="text-align:right;width: 15em; "> 11.97 </td>
   <td style="text-align:right;width: 15em; "> 8.50 </td>
   <td style="text-align:right;width: 15em; font-weight: bold;"> 7.67 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 120em; "> Average number of posters in queue (WIQ) </td>
   <td style="text-align:right;width: 15em; font-weight: bold;"> 6.64 </td>
   <td style="text-align:right;width: 15em; "> 8.77 </td>
   <td style="text-align:right;width: 15em; "> 6.98 </td>
   <td style="text-align:right;width: 15em; "> 4.16 </td>
   <td style="text-align:right;width: 15em; "> 0.68 </td>
   <td style="text-align:right;width: 15em; font-weight: bold;"> 1.02 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 120em; "> Average wait time (WIP) (in Minutes) </td>
   <td style="text-align:right;width: 15em; font-weight: bold;"> 124.43 </td>
   <td style="text-align:right;width: 15em; "> 154.69 </td>
   <td style="text-align:right;width: 15em; "> 117.37 </td>
   <td style="text-align:right;width: 15em; "> 102.61 </td>
   <td style="text-align:right;width: 15em; "> 72.83 </td>
   <td style="text-align:right;width: 15em; font-weight: bold;"> 65.77 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 120em; "> Average wait time in queue waiting for service (WIQ) (in Minutes) </td>
   <td style="text-align:right;width: 15em; font-weight: bold;"> 56.93 </td>
   <td style="text-align:right;width: 15em; "> 75.19 </td>
   <td style="text-align:right;width: 15em; "> 59.87 </td>
   <td style="text-align:right;width: 15em; "> 35.61 </td>
   <td style="text-align:right;width: 15em; "> 5.83 </td>
   <td style="text-align:right;width: 15em; font-weight: bold;"> 8.77 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 120em; "> Cost of in-process inventory waiting for service (WIQ) ($ Per Hour) </td>
   <td style="text-align:right;width: 15em; font-weight: bold;"> 53.14 </td>
   <td style="text-align:right;width: 15em; "> 70.18 </td>
   <td style="text-align:right;width: 15em; "> 55.88 </td>
   <td style="text-align:right;width: 15em; "> 33.24 </td>
   <td style="text-align:right;width: 15em; "> 5.45 </td>
   <td style="text-align:right;width: 15em; font-weight: bold;"> 8.19 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;color: white !important;background-color: FireBrick !important;width: 120em; "> Cost of all in-process inventory (WIP) ($ Per Hour) </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: FireBrick !important;width: 15em; font-weight: bold;"> 116.14 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: FireBrick !important;width: 15em; "> 144.38 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: FireBrick !important;width: 15em; "> 109.54 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: FireBrick !important;width: 15em; "> 95.78 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: FireBrick !important;width: 15em; "> 67.98 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: FireBrick !important;width: 15em; font-weight: bold;"> 61.38 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 120em; "> Cost of servers </td>
   <td style="text-align:right;width: 15em; font-weight: bold;"> 66.00 </td>
   <td style="text-align:right;width: 15em; "> 71.60 </td>
   <td style="text-align:right;width: 15em; "> 60.75 </td>
   <td style="text-align:right;width: 15em; "> 68.00 </td>
   <td style="text-align:right;width: 15em; "> 87.00 </td>
   <td style="text-align:right;width: 15em; font-weight: bold;"> 81.75 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;color: white !important;background-color: SteelBlue !important;width: 120em; "> Total Cost ($ Per Hour) </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: SteelBlue !important;width: 15em; font-weight: bold;"> 182.14 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: SteelBlue !important;width: 15em; "> 215.98 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: SteelBlue !important;width: 15em; "> 170.29 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: SteelBlue !important;width: 15em; "> 163.78 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: SteelBlue !important;width: 15em; "> 154.98 </td>
   <td style="text-align:right;font-weight: bold;color: white !important;background-color: SteelBlue !important;width: 15em; font-weight: bold;"> 143.13 </td>
  </tr>
</tbody>
<tfoot>
<tr><td style="padding: 0; border: 0;" colspan="100%"><span style="font-style: italic;">Note: </span></td></tr>
<tr><td style="padding: 0; border: 0;" colspan="100%">
<sup></sup> - Proposal 1: Decrease printing rate</td></tr>
<tr><td style="padding: 0; border: 0;" colspan="100%">
<sup></sup> - Proposal 1b: Increase printing rate</td></tr>
<tr><td style="padding: 0; border: 0;" colspan="100%">
<sup></sup> - Proposal 2: Senior Inspector</td></tr>
<tr><td style="padding: 0; border: 0;" colspan="100%">
<sup></sup> - Proposal 2b: 2 Senior Inspectors</td></tr>
<tr><td style="padding: 0; border: 0;" colspan="100%">
<sup></sup> - Proposal 3: Increase printing rate and 2 Senior Inspectors</td></tr>
</tfoot>
</table><!--/html_preserve-->


## Save Results


```r
write.csv(total, file = "../Final_Project/Data/print_squo.csv", row.names = TRUE)
write.csv(prop_1, file = "../Final_Project/Data/print_prop_1.csv", row.names = TRUE)
write.csv(prop_1b, file = "../Final_Project/Data/print_prop_1b.csv", row.names = TRUE)
write.csv(prop_2, file = "../Final_Project/Data/print_prop_2.csv", row.names = TRUE)
write.csv(prop_2b, file = "../Final_Project/Data/print_prop_2b.csv", row.names = TRUE)
write.csv(prop_3, file = "../Final_Project/Data/print_prop_3.csv", row.names = TRUE)
```

