#Installation of Packages
install.packages("ggplot2")
library(ggplot2)
install.packages("tidyverse")
library(tidyverse)
install.packages("ggpubr")
library(ggpubr)
install.packages("ggsignif")
library(ggsignif)
install.packages("arsenal")
library(arsenal)


#Separating PackYears into Categories
lung_cancer <- lung_cancer %>%  
  mutate(category_packyears = case_when(      
    
    packyears == 0.0 ~ "Never Smokers",
    packyears <= 20.0 ~ "Light Smokers",
    packyears >= 20.0 & packyears <= 40.0 ~ "Moderate Smokers",    
    packyears >= 40.0 ~ "Heavy Smokers",
    TRUE ~ "Unknown"
    # Catch-all for unexpected cases
  ))

#Seperating BMI's into BMI Categories
lung_cancer <- lung_cancer %>%  
  mutate(BMI_classification = case_when(      
    
    BMI <= 20.0 ~ "UnderWeight",
    BMI <= 24.9 ~ "Normal",     
    BMI >= 25.0 & BMI <= 29.9 ~ "Overweight",
    BMI >= 30.0 ~ "Obese",
    
    # Catch-all for unexpected cases
  ))



# Pie Chart of Smoker Type Categories
lung_cancer$category_packyears <- factor(lung_cancer$category_packyears, levels = c("Never Smokers", "Light Smokers", "Moderate Smokers", "Heavy Smokers"))
category_packyears <- lung_cancer %>%
  count(category_packyears)%>%  
  mutate(Percentage = n / sum(n) * 100,  # Calculate percentage         
         Label = paste0(category_packyears, " ", round(Percentage, 1), "%"))  # Format label
ggplot(category_packyears, aes(x = "", y = n, fill = category_packyears)) +  
  geom_bar(stat = "identity", width = 1) +  # Create bars (converted to pie slices)
coord_polar("y", start = 0) +  # Convert to pie chart  
  labs(title = "Smoker Type Distribution", x = NULL, y = NULL) +  
  theme_void() +  # Remove axis and gridlines  
  scale_fill_manual(values = c("Never Smokers" = "lightgreen", "Light Smokers" = "yellow" , "Moderate Smokers" = "orange", "Heavy Smokers" = "red")) +  
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 4, color = "black")  # Add percentage labels




# Convert BMI_Classification column to a factor with correct ordered levels
lung_cancer$BMI_classification <- factor(lung_cancer$BMI_classification, levels = c("UnderWeight", "Normal", "Overweight", "Obese"))
lung_cancer$category_packyears <- factor(lung_cancer$category_packyears, levels = c("Never Smokers", "Light Smokers", "Moderate Smokers", "Heavy Smokers"))
#Boxplots showing relationship between smoking Years, Survival Years, and BMI Category
ggplot(lung_cancer, aes(x = BMI_classification, y = survival_years, fill = category_packyears)) +     
geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.color = "black") +  # Adjust transparency & highlight outliers  
scale_fill_manual(values = c("Never Smokers" = "lightgreen", "Light Smokers" = "yellow", "Moderate Smokers" = "orange", "Heavy Smokers" = "red")) +  # Custom color mapping for alcohol use  
labs(title = "Relationship Between BMI Category, Type of Smokers, and Survival Years",    
x = "BMI Category",    
y = "Survival Years",    
fill = "Type of Smoker") +
theme_bw() +
stat_compare_means(method = "t.test", label = "p.signif", comparisons = list(c("Normal", "UnderWeight"), c("Normal", "Overweight"), c("Normal", "Obese")))
                                         
  
# Histogram of Pack Years
lung_cancer$BMI_classification <- factor(lung_cancer$BMI_classification, levels = c("UnderWeight", "Normal", "Overweight", "Obese")
ggplot(lung_cancer, aes(x = packyears)) +  
geom_histogram(bins = 10, fill = "blue", color = "black", alpha = 0.2) + 
facet_wrap(~factor(BMI_classification, levels = c("UnderWeight", "Normal", "Overweight", "Obese"))) +
labs(title = "Histogram of Pack Years Amongst BMI Groups", x = "Pack Years", y = "Frequency") +  
theme_bw(base_size = 14)


#Density Plot of Tumor Size
lung_cancer$BMI_classification <- factor(lung_cancer$BMI_classification, levels = c("UnderWeight", "Normal", "Overweight", "Obese")
ggplot(lung_cancer, aes(x = tumor_size_cm)) +  
geom_density(fill = "purple", alpha = 0.3) +
facet_wrap(~factor(BMI_classification, levels = c("UnderWeight", "Normal", "Overweight", "Obese"))) +  
labs(title = "Density Plots of Tumor Size (cm) Amongst BMI Groups", 
x = "Tumor Size (cm)", y = "Density") +  
theme_bw(base_size = 14)


#Scatterplot showing relationship between Env Exposure Index, Tumor Size, and BMI
lung_cancer$category_packyears <- factor(lung_cancer$category_packyears, levels = c("Never Smokers", "Light Smokers", "Moderate Smokers", "Heavy Smokers")
ggplot(lung_cancer, aes(x = BMI , y = tumor_size_cm)) +  geom_point() +
geom_point(alpha = 0.4, size = 2) +
geom_smooth(method = "lm", se = TRUE, linetype = "solid", size = 1.2, alpha = 0.3) +  # Regression line with confidence interval  
facet_wrap(~factor(category_packyears, levels = c("Never Smokers", "Light Smokers", "Moderate Smokers", "Heavy Smokers"))) +  #ncol by default; ncol = 2 arranges facets into 2 columns.
  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "top", size = 5) +
labs(title = "Relationship Between Smoker Type, BMI, and Tumor Size", x = "BMI", y = "Tumor Size (cm)") + 
theme_bw()


#Creation of a Summary Report With T-test and Chi-Square Test
controls_report <- tableby.control(test = FALSE, total = FALSE,
                                   numeric.test= "t", cat.test= "chisq",
                                   numeric.stats = c("Nmiss", "median", "q1q3",
                                                     "range"),
                                   cat.stats = c("Nmiss","countpct"),
                                   stats.labels = list(median="Median", q1q3="Q
                            1,Q3", range = "Range"))
#Create Labels
report_labels = c(year_diag = "Year Diagnosed",
                  age = "Age",
                  Sex = "Sex",
                  BMI_classification = "BMI Category",
                  smoker = "Smoking Category",
                  packyears = "Smoking Years",
                  env_exposure_idx = "Env Exposure",
                  tumor_size_cm = "Tumor Size(cm)")
statistical_report <- tableby( ~ year_diag + age + sex + BMI_classification + smoker + packyears + env_exposure_idx + tumor_size_cm
                
                               ,
                               
                               data=lung_cancer ,
                               digits=1 ,
                               test=TRUE,
                               digits.p=2,
                               total = T,
                               control=controls_report)
summary(statistical_report,
        test = TRUE,
        labelTranslations = report_labels,
        control = controls_report,
        title = "Summary: Overall", text = TRUE)
write2word(statistical_report,labelTranslations=report_labels,("Report_summary.doc"))















