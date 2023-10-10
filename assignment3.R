## Problem 2

# a

Aside from English, what language is most common for films? Answer this with a single SQL query.

```{r}
setwd("/Users/dalaohuhan/Downloads")
library(DBI)
library(RSQLite)
sakila <- dbConnect(RSQLite::SQLite(),
                    "sakila_master.db")
sa <- function(x) {
  dbGetQuery(sakila,x)
}
sa("
   SELECT language_id, COUNT(*)
   FROM film
   ")
```

# b

What genre of movie is the most common in the data, and how many movies are of this genre?
  
  ```{r}
genre <- sa(" 
   SELECT f.film_id, c.name
   FROM film_category AS f
        INNER JOIN category AS c ON c.category_id = f.category_id
   ")
ag1 <- aggregate(genre$film_id, by = list(name = genre$name), length)
ag1[order(-ag1$x),]
```

```{r}
sa(" 
   SELECT COUNT(f.film_id) AS count, c.name
   FROM film_category AS f
        INNER JOIN category AS c ON c.category_id = f.category_id
   GROUP BY name
   ORDER BY -count
   ")
```

# c

Identify which country or countries have exactly 9 customers. Answer this with a single SQL query.

```{r}
c_country <- sa("
   SELECT c.customer_id, co.country
   FROM customer AS c
        LEFT JOIN address AS a ON a.address_id = c.address_id
             LEFT JOIN city AS ci ON ci.city_id = a.city_id
                  LEFT JOIN country AS co ON co.country_id = ci.country_id
   ")
ag2 <- aggregate(c_country$customer_id, 
                 by = list(country = c_country$country), length)
ag2$country[which(ag2$x == 9)]
```

```{r}
sa("
   SELECT COUNT(c.customer_id) AS count, co.country
   FROM customer AS c
        LEFT JOIN address AS a ON a.address_id = c.address_id
             LEFT JOIN city AS ci ON ci.city_id = a.city_id
                  LEFT JOIN country AS co ON co.country_id = ci.country_id
   GROUP BY country
   ORDER BY -count
   LIMIT 20
   ")
```

## Problem 3 - US Records

# a

What proportion of email addresses are hosted at a domain with TLD ".net"? (E.g. in the email, "angrycat\@freemail.org", "freemail.org" is the domain, with TLD (top-level domain) ".org".)

```{r}
setwd("/Users/dalaohuhan/Downloads")
us500 <- read.csv("us-500.csv")
num_net <- length(grep(".net",us500$email))
num_net/500
```

# b

What proportion of email addresses have at least one non alphanumeric character in them?
  
  ```{r}
email <- gsub("@.*$","",us500$email)
num_non_alphanumeric <- length(grep("[^[:alnum:]]", email))
num_non_alphanumeric/500
```

# c

What is the most common area code amongst all phone numbers?
  
  ```{r}
p1 <- substr(us500$phone1,1,3)
p2 <- substr(us500$phone2,1,3)
all.equal(p1,p2)
code <- table(p1)
code[which.max(code)]
```

# d

Produce a histogram of the log of the apartment numbers for all addresses. (You may assume any number after the street is an apartment number.)

```{r}
library(stringr)
#extract the number after # 
address1 <- str_extract(us500$address, "(?<=#)\\d+$") 
#extract the numbers with Spaces on both sides from the rest of the address
address2 <- str_extract(us500$address[is.na(address1)],
                        "(?<!^)(?<=[:alpha:] )\\d+\\b")
ap1 <- as.numeric(address1)
ap2 <- as.numeric(address2)
ap <- na.omit(c(ap1,ap2))
hist(log(ap),col="lightblue")

```

# e

Benford's law is an observation about the distribution of the leading digit of real numerical data. Examine whether the apartment numbers appear to follow Benford's law. Do you think the apartment numbers would pass as real data?
  
  ```{r}
leading_digit <- as.numeric(substr(ap, 1, 1))
ap_freq <- table(leading_digit)
expected_freq <- log10(1 + 1 / (1:9))
chisq.test(ap_freq, p = expected_freq)
```



# f

Repeat your analysis of Benford's law on the last digit of the street number. (E.g. if your address is "123 Main St #25", your street number is "123".)

```{r}
#extract the first few numbers in the address
street <-gsub(".*?(\\d+).*", "\\1", us500$address)
street_num <- as.numeric(street)
#extract the last digit of the street
street_last_dig <- as.numeric(substring(street_num, 
                                        nchar(street_num) - 0, nchar(street_num)))
#delect 0
last_not_0_digit <- street_last_dig[street_last_dig !=0]
street_freq <- table(last_not_0_digit)
expected_freq <- log10(1 + 1 / (1:9))
chisq.test(street_freq, p = expected_freq)
```
