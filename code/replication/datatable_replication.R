# Learning some data table

# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html

# Just running the vignette

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  data.table
)

input <- if (file.exists("flights14.csv")) {
  
  "flights14.csv"
} else {
  "https://raw.githubusercontent.com/Rdatatable/data.table/master/vignettes/flights14.csv"
}
flights <- fread(input)
flights
# year month   day dep_delay arr_delay carrier origin   dest air_time distance  hour
# <int> <int> <int>     <int>     <int>  <char> <char> <char>    <int>    <int> <int>
#   1:  2014     1     1        14        13      AA    JFK    LAX      359     2475     9
# 2:  2014     1     1        -3        13      AA    JFK    LAX      363     2475    11
# 3:  2014     1     1         2         9      AA    JFK    LAX      351     2475    19
# 4:  2014     1     1        -8       -26      AA    LGA    PBI      157     1035     7
# 5:  2014     1     1         2         1      AA    JFK    LAX      350     2475    13
# ---                                                                                    
#   253312:  2014    10    31         1       -30      UA    LGA    IAH      201     1416    14
# 253313:  2014    10    31        -5       -14      UA    EWR    IAH      189     1400     8
# 253314:  2014    10    31        -8        16      MQ    LGA    RDU       83      431    11
# 253315:  2014    10    31        -4        15      MQ    LGA    DTW       75      502    11
# 253316:  2014    10    31        -5         1      MQ    LGA    SDF      110      659     8

#Aside: fread accepts http and https URLs directly as well as operating system commands such as sed and awk output. See ?fread for examples.

?fread

# 1) Basics ----

DT = data.table(
  ID = c("b","b","b","a","a","c"),
  a = 1:6,
  b = 7:12,
  c = 13:18
)
DT
#        ID     a     b     c
#    <char> <int> <int> <int>
# 1:      b     1     7    13
# 2:      b     2     8    14
# 3:      b     3     9    15
# 4:      a     4    10    16
# 5:      a     5    11    17
# 6:      c     6    12    18
class(DT$ID)
# [1] "character"

# You can also convert existing objects to a data.table using setDT() (for data.frames and lists) and as.data.table() (for other structures); the difference is beyond the scope of this vignette, see ?setDT and ?as.data.table for more details.

?setDT
# setDT is faster than as.data.table() because the latter makes a copy of the input object

getOption("datatable.print.nrows")

#DT[i, j, by]

# R: i j by
# SQL: where | order by  select | update group by

# Take DT, subset/reorder rows using i, then calculate j, grouped by by.

# c) Subset rows in i
## get all fligths with "JFK" as the origin airport in the onth of June
## 

ans <- flights[origin=="JFK" & month== 6L]
head(ans)

# can refer to cols as if they're variables, but flights$origin and flights$month would also work

# row indices satisfying the conditions origin==JFK and month ==6L are computed. Since there's nothing else left to do, all cols from flights at these rows are just returned as a DT

# comma after the condition i isn't required, but
# flights[origin == "JFK" & month == 6L, ] would also work

# get the first two rows from flights

ans <- flights[1:2]

ans


# no condition, row indices are already provided in i so we return the DT with all cols from flights at rows for these row indices

# sort flights first by column origin in ascending order, then by dest in descending order using R's order() function

ans <- flights[order(origin, -dest)]
ans

# d) Select column(s) in j

# select arr_delay column, but return it as a vector

ans <- flights[,arr_delay]

head(ans)

#[1]  13  13   9 -26   1   0

# since cols can be referred to like vars, directly refer to the variable we want to subset. Since we want all rows, skip the i part
# # returns all rows for the column arr_delay
#

# select arr_delay column, but return as a data.table instead
#

ans <- flights[, list(arr_delay)]

head(ans)

# arr_delay
# <int>
#   1:        13
# 2:        13
# 3:         9
# 4:       -26
# 5:         1
# 6:         0

# wrap the varialbes (col names) with list() which ensures a DT is returned. If there's a single col name, not wrapping with list() returns a vector instead
# you can also wrap cols with .() instead of list(); they mean the same thing

# data tables and frames are internally lists, requiring that each element has the same length and the list has a class attribute. 

# allowing j to return a list enables converting and returning DTs efficiently
# as long as the j-expression returns a list, each element of that list will be a column in the resulting DT. 

# select both arr_delay and dep_delay columns

ans <- flights[, .(arr_delay, dep_delay)]

head(ans)
# arr_delay dep_delay
# <int>     <int>
#   1:        13        14
# 2:        13        -3
# 3:         9         2
# 4:       -26        -8
# 5:         1         2
# 6:         0         4

# alternatively
ans <- flights[, list(arr_delay, dep_delay)]

# select both arr_delay and dep_delay columns, AND rename them to delay_arr and delay_dep
# because .() == list(), we can rename columns as we would while creating a list

ans <- flights[, .(delay_arr = arr_delay, delay_dep = dep_delay)]

head(ans)
# delay_arr delay_dep
# <int>     <int>
#   1:        13        14
# 2:        13        -3
# 3:         9         2
# 4:       -26        -8
# 5:         1         2
# 6:         0         4

# e) Compute or DO in j
# how many trips have had a total delay < 0?

ans <- flights[, sum((arr_delay + dep_delay) < 0)]
ans
# 141814
# 
# 

# DT's j can do more than just select columns, it can handle expressions or computing on columns, and then we can compute by calling functions on those variables

# f) Subset in i AND do in j
# # calculate the average arrival and departure delay for all flights with JFK as the origin in the month of June

ans <- flights[origin == "JFK" & month == 6L,
               .(m_arr = mean(arr_delay),
                 m_dep = mean(dep_delay))]

ans
# m_arr    m_dep
# <num>    <num>
#   1: 5.839349 9.80788

# first subset in i to find matching row indices where origin airport == JFK, and month == 6L
# Don't subset the entire DT corresponding to these rows yet
# 2) Look at j, and see it only uses 2 columns and that we're computing the mean, so just subset the cols corresponding to the matching rows and compute the mean

# Because (i,j,by) are together inside [...], DT can see all three and optmize the query before evaluation, not separately. We can therefore avoid the entire subset (i.e. by subsetting cols besides arr_delay and dep_delay for both speed and memory efficiency)

# How many trips made in 2014 fro JFK in the month of June?
# 
ans <- flights[origin == "JFK" & month == 6L, length(dest)]
ans
# 8422

# lenght() requires an input argument, we just needed to compute the number of rows in the subset, so could have used any other column as the input arg to length()
# This type of op is frequently, esp. while grouping, so DT provides the symbol .N for it
# 
# ## Special symbol .N
# It's a special built-in var that holds the number of obs in the CURRENT GROUP, esp. useful combined with "by"
# Without any group by operations, it just gives you the number of rows in the subset

ans <- flights[origin == "JFK" & month == 6L, .N]
ans
# 8422

# 1) subset in i to get tthe row indices where origin == JFK, month == June
# 2) See that j only uses .N, no other columns, so we don't show the entire subset, just ge tthe number of rows in the subset == the length of row indices
# 3) because we didn't wrap .N with list() or .(), a vector got returned

# could also have done: 
# nrow(flights[origin == "JFK" & month == 6L]) but this would first subset the entire DT corresponding to the row indices in i, then return the rows, which is inefficient

# g) refer to cols by names in j (like data.frame)
# if you're writing colnames explicitly, no difference
# Select both arr_delay and dep_delay in the data.frame way:

ans <- flights[, c("arr_delay","dep_delay")]
head(ans)
# arr_delay dep_delay
# <int>     <int>
#   1:        13        14
# 2:        13        -3
# 3:         9         2
# 4:       -26        -8
# 5:         1         2
# 6:         0         4

# if you've stored the desired cols in a character vector, you can use the .. prefix or the with argument:

select_cols <- c("arr_delay","dep_delay")

flights[, ..select_cols]

# arr_delay dep_delay
# <int>     <int>
#   1:        13        14
# 2:        13        -3
# 3:         9         2
# 4:       -26        -8
# 5:         1         2
# ---                    
#   253312:       -30         1
# 253313:       -14        -5
# 253314:        16        -8
# 253315:        15        -4
# 253316:         1        -5

# this .. is like the "up-one-level" command: the .. signals to DT to look for the select_cols variable "up-one-level" i.e. here in the global environment
#

# select columns named in a variable using with = FALSE

# this is going to "data frame mode"
flights[,select_cols, with = FALSE]

# flights[,select_cols] does not work
# with is named after R's with(). In base R you could do:


DF = data.frame(x = c(1,1,1,2,2,3,3,3), y = 1:8)

## (1) normal way
DF[DF$x > 1, ] # data.frame needs that ',' as well
#   x y
# 4 2 4
# 5 2 5
# 6 3 6
# 7 3 7
# 8 3 8

## (2) using with
DF[with(DF, x > 1), ]
#   x y
# 4 2 4
# 5 2 5
# 6 3 6
# 7 3 7
# 8 3 8
# (2) using with() allows you to use DF's column x as if it were a variable

# with=FALSE disables the ability to refer to cols as if they're variables, restorign the "data.frame mode"

# also can deselect cols using - or !:

ans <- flights[, !c("arr_delay","dep_delay")]
ans <- flights[, -c("arr_delay","dep_delay")]

# also can select by specifying start and end col names, e.g. year:day to select the first 3
# 
# return year,month,day

ans <- flights[, year:day]

# returns day,month,year
ans <- flights[, day:year]

# return all cols except year, month, day
ans <- flights[, -(year:day)]
ans <- flights[, !(year:day)]

# with = TRUE is the default because letting j handle expressions is more handy

# 2) Aggregations ----

# Let's combine i and j to perform operations by group
## a) Grouping using by ----

### Get the number of trips corresponding to each origin airport ----

ans <- flights[, .(.N), by = .(origin)]
# equivalently
ans <- flights[, .(.N), by = "origin"]

ans

# origin     N
# <char> <int>
#   1:    JFK 81483
# 2:    LGA 84433
# 3:    EWR 87400

# .N is a special var that holds the number of rows of the current group. Grouping by origin gets the number of rows for each group
# by doing head(flights) you can see that origin airports occurs in the order JFK LGA EWR, and this original order of grouping variables is preserved

# because there was no colname given for the column returned, it was named N automatically by recogzning the .N symbol
# by also accepts a character vector of colnames. This is useful for coding programmatically e.g designing a function with the grouping columns as a (character vector) function argument

# when there's 1 col or expression to refer to in j and by, we can drop the .() notation for convenience: 

ans <- flights[, .N, by = origin]
ans

### Calculate the number of trips for each origin airport for carrier code "AA" ----

ans <- flights[carrier == "AA", .N, by = origin]

ans
#    origin     N
#    <char> <int>
# 1:    JFK 11923
# 2:    LGA 11730
# 3:    EWR  2649

# 1) obtain row indices for the expression carrier=="AA" from i
# 2) Using those row indices, obtain the N rows while grouped by origin. Again, no cols ar eused here because j doesn't require any expressions to be subsetted

### Get total number of trips for each origin, dest pair for carrier code "AA" ----

ans <- flights[carrier == "AA", 
               .N,
               by = .(origin, dest)]
head(ans)
# origin   dest     N
# <char> <char> <int>
#   1:    JFK    LAX  3387
# 2:    LGA    PBI   245
# 3:    EWR    LAX    62
# 4:    JFK    MIA  1876
# 5:    JFK    SEA   298
# 6:    EWR    MIA   848

## equivalently with a character vector in 'by':
ans <- flights[carrier == "AA",
               .N,
               by = c("origin","dest")]

# by accepts multiple columns, can use .() or list()

### Average arrival and departure delay for each origin,dest pair for each month for carrier code "AA"

ans <- flights[carrier == "AA",
               .(mean(arr_delay),mean(dep_delay)),
               by = .(origin, dest,month)]

ans

#      origin   dest month         V1         V2
#      <char> <char> <int>      <num>      <num>
#   1:    JFK    LAX     1   6.590361 14.2289157
#   2:    LGA    PBI     1  -7.758621  0.3103448
#   3:    EWR    LAX     1   1.366667  7.5000000
#   4:    JFK    MIA     1  15.720670 18.7430168
#   5:    JFK    SEA     1  14.357143 30.7500000
#  ---                                          
# 196:    LGA    MIA    10  -6.251799 -1.4208633
# 197:    JFK    MIA    10  -1.880184  6.6774194
# 198:    EWR    PHX    10  -3.032258 -4.2903226
# 199:    JFK    MCO    10 -10.048387 -1.6129032
# 200:    JFK    DCA    10  16.483871 15.5161290

# because no colnames were specified for j, V1 and V2 were automatically generated
# input order of grouping columns is preserved

# rename output cols:
ans <- flights[carrier == "AA",
               .(mean_arr_delay = mean(arr_delay),mean_dep_delay = mean(dep_delay)),
               by = .(origin, dest,month)]

ans

# What if we want to order the result by the grouping cols origin, dest and month?

## b) Sorted by: keyby ----

# DT retaining the original order of groups is intentional, but sometimes we want an automatic sort

### Directly order all the grouping variables

ans <- flights[carrier== "AA",
               .(mean(arr_delay), mean(dep_delay)),
               keyby = .(origin,dest,month)]

ans 
# Key: <origin, dest, month>
#      origin   dest month         V1         V2
#      <char> <char> <int>      <num>      <num>
#   1:    EWR    DFW     1   6.427673 10.0125786
#   2:    EWR    DFW     2  10.536765 11.3455882
#   3:    EWR    DFW     3  12.865031  8.0797546
#   4:    EWR    DFW     4  17.792683 12.9207317
#   5:    EWR    DFW     5  18.487805 18.6829268
#  ---                                          
# 196:    LGA    PBI     1  -7.758621  0.3103448
# 197:    LGA    PBI     2  -7.865385  2.4038462
# 198:    LGA    PBI     3  -5.754098  3.0327869
# 199:    LGA    PBI     4 -13.966667 -4.7333333
# 200:    LGA    PBI     5 -10.357143 -6.8571429

# changing to keyby automatically orders the result by the grouping vars in increasing order, and this is often faster: by internally requires a sort before recovering the original table's order

# keys: keyby also sets a key after ordering by setting an attribute called sorted

## c) Chaining ----

# Reconsider the "get the total number of grips for each origin, dest pair for carrier "AA"

ans <- flights[carrier == "AA",
               .N,
               by = .(origin, dest)]

# origin   dest     N
# <char> <char> <int>
#   1:    JFK    LAX  3387
# 2:    LGA    PBI   245
# 3:    EWR    LAX    62
# 4:    JFK    MIA  1876
# 5:    JFK    SEA   298
# 6:    EWR    MIA   848

### Order ans using the columns origin in ascending order, dest in descending order

# store the intermediate result as ans, then use order(origin, -dest) on that variable:

ans <- ans[order(origin,-dest)]

head(ans)
# origin   dest     N
# <char> <char> <int>
#   1:    EWR    PHX   121
# 2:    EWR    MIA   848
# 3:    EWR    LAX    62
# 4:    EWR    DFW  1618
# 5:    JFK    STT   229
# 6:    JFK    SJU   690

# remeber we can use - on a character column in order() within the frame of a data.table b/c of DT's internal quer optimization

# recall that order(...) within the frame of a DT is automatcialy optiized using DT's fast order

# but this requires assigning the intermediate result and then overwriting it. We could instead chain expressions.

ans <- flights[carrier == "AA",
               .N,
               by = .(origin,dest)][order(origin,-dest)]

head(ans,10)
#     origin   dest     N
#     <char> <char> <int>
#  1:    EWR    PHX   121
#  2:    EWR    MIA   848
#  3:    EWR    LAX    62
#  4:    EWR    DFW  1618
#  5:    JFK    STT   229
#  6:    JFK    SJU   690
#  7:    JFK    SFO  1312
#  8:    JFK    SEA   298
#  9:    JFK    SAN   299
# 10:    JFK    ORD   432

# We can tack expressions one after another, forming a chain of operations
# ie DT[ ... ][ ... ][ ... ]
# or vertically:
# DT[ ...
# ][ ...
# ][ ...
# ]

## d) Expressions in by ----
### Can by accept expressions as well, or does it just take columns?

# Obvs yes, e.g. how many flights started late but arrived early (or on time):

ans <- flights[,
               .N,
               .(dep_delay >0,
                 arr_delay > 0)]

ans
# dep_delay arr_delay      N
# <lgcl>    <lgcl>  <int>
#   1:      TRUE      TRUE  72836
# 2:     FALSE      TRUE  34583
# 3:     FALSE     FALSE 119304
# 4:      TRUE     FALSE  26593

# row 4 here is dep_delay >0 == TRUE and arr_delay >0 == FALSE, so 25k started late but arrived early or on time
# no names were given to the by-expression, so names were automatically assigned; but you could name them:

ans <- flights[,
               .N,
               .(dep_delayed = dep_delay > 0,
                 arr_delayed = arr_delay > 0)]

ans
# dep_delayed arr_delayed      N
# <lgcl>      <lgcl>  <int>
#   1:        TRUE        TRUE  72836
# 2:       FALSE        TRUE  34583
# 3:       FALSE       FALSE 119304
# 4:        TRUE       FALSE  26593

# can also provide other columns along with expressions:

ans <- flights[,
               .N,
               .(dep_delayed = dep_delay > 0,
                 arr_delayed = arr_delay >0,
                 origin)][
                   order(origin,dep_delayed,arr_delayed)
                 ]

ans
# 
# <lgcl>      <lgcl> <char> <int>
#   1:       FALSE       FALSE    EWR 38082
# 2:       FALSE        TRUE    EWR  9298
# 3:        TRUE       FALSE    EWR 11265
# 4:        TRUE        TRUE    EWR 28755
# 5:       FALSE       FALSE    JFK 38631
# 6:       FALSE        TRUE    JFK 12749
# 7:        TRUE       FALSE    JFK  8216
# 8:        TRUE        TRUE    JFK 21887
# 9:       FALSE       FALSE    LGA 42591
# 10:       FALSE        TRUE    LGA 12536
# 11:        TRUE       FALSE    LGA  7112
# 12:        TRUE        TRUE    LGA 22194

## e) Multiple columns in j - .SD ----
### Do we have to compute mean() for each column individually?
# Typing mean(myCol) for every column isn't practical, what if you had 100 cols?
# Recall: as long as the j expression returns a list, each element of the list is converted to a column in the resulting data table
# suppose we can refer to the data subset for each group as a variable WHILE grouping, then loop through all cols of that variable using lappy()


# Special symbol .SD:
# .SD is Subset of Data, it's a data table holding the data for the current group defined using by
# Recall that data.table is internally a list with all columns of equal length

DT = data.table(
  ID = c("b","b","b","a","a","c"),
  var_1 = 1:6,
  var_2 = 7:12,
  var_3 = 13:18
)

DT
#        ID var_1 var_2 var_3
#     <char> <int> <int> <int>
# 1:      b     1     7    13
# 2:      b     2     8    14
# 3:      b     3     9    15
# 4:      a     4    10    16
# 5:      a     5    11    17
# 6:      c     6    12    18

DT[,
   print(.SD),
   by = ID]

# ID == b
#      var_1 var_2 var_3
#      <int> <int> <int>
# 1:     1     7    13
# 2:     2     8    14
# 3:     3     9    15

# ID == a
#      var_1 var_2 var_3
#      <int> <int> <int>
# 1:     4    10    16
# 2:     5    11    17

# ID == c
#     var_1 var_2 var_3
#     <int> <int> <int>
# 1:     6    12    18
# Empty data.table (0 rows and 1 cols): ID

# .SD contains all cols except the grouping cols by default
# it also preserves the original order: first b then a then c

# To compute on multiple columns, use base R's lapply()

DT[, 
   lapply(.SD, mean),
   by = ID]

#        ID  var_1 var_2 var_3
#    <char> <num> <num> <num>
# 1:      b   2.0   8.0  14.0
# 2:      a   4.5  10.5  16.5
# 3:      c   6.0  12.0  18.0

# .SD holds the rows corresponding to cols var_1, var_2 and var_3 for that group
# compute mean() on each of these columns using lapply()

# each group returns a list of 3 elements containing the means which becomes the cols of the resulting data.table

# because lapply() returns a list, don't need to wrap with an additional .()

# suppose we just want the mean() of arr_delay and dep_delay. .SD would contain ALL columns other than the grouping variables

### Specify just the columns we'd like to compute the mean() on
# .SDcols
# This argument accepts either colnames or col indices, e.g. .SDcols = c("arr_delay","dep_delay") would have .SD containing only these columns for that group

# YHou can also provide cols to remove rather than cols to keep with - or !, or colA:colB or deselect !(colA:colB) or -(colA:colB)

# Use .SD along with .SDcols to get mean() of arr_Delay and dep_delay grouped by origin, dest and month:

flights[carrier == "AA",                       ## only trips wtih carrier AA,
        lapply(.SD, mean),                     ## only compute the mean
        by = .(origin, dest, month),           ## for every origin,dest,month
        .SDcols = c("arr_delay","dep_delay")   ## for just those specified in .SDcols
        ]

#      origin   dest month  arr_delay  dep_delay
#      <char> <char> <int>      <num>      <num>
#   1:    JFK    LAX     1   6.590361 14.2289157
#   2:    LGA    PBI     1  -7.758621  0.3103448
#   3:    EWR    LAX     1   1.366667  7.5000000
#   4:    JFK    MIA     1  15.720670 18.7430168
#   5:    JFK    SEA     1  14.357143 30.7500000
#  ---                                          
# 196:    LGA    MIA    10  -6.251799 -1.4208633
# 197:    JFK    MIA    10  -1.880184  6.6774194
# 198:    EWR    PHX    10  -3.032258 -4.2903226
# 199:    JFK    MCO    10 -10.048387 -1.6129032
# 200:    JFK    DCA    10  16.483871 15.5161290

## f) Subset .SD for each group ----

### Return the first two rows for each month ----

ans <- flights[,
               head(.SD, 2),
               by = month]

head(ans)
#    month  year   day dep_delay arr_delay carrier origin   dest air_time distance  hour
#    <int> <int> <int>     <int>     <int>  <char> <char> <char>    <int>    <int> <int>
# 1:     1  2014     1        14        13      AA    JFK    LAX      359     2475     9
# 2:     1  2014     1        -3        13      AA    JFK    LAX      363     2475    11
# 3:     2  2014     1        -1         1      AA    JFK    LAX      358     2475     8
# 4:     2  2014     1        -5         3      AA    JFK    LAX      358     2475    11
# 5:     3  2014     1       -11        36      AA    JFK    LAX      375     2475     8
# 6:     3  2014     1        -3        14      AA    JFK    LAX      368     2475    11

# .SD is a data table that holds all rows for THAT group; just subset the first two rows
# for each group, head(.SD, 2) returns first two rows as a data table which is also a list so it doesn't need to be wrapped with .()

## g) Why keep j so flexible? ----
# to get consistent syntax and allow for using existing base functions rather than new functions

### Concatenate cols var_1 and var_2 for each group in ID ----

DT
# ID var_1 var_2 var_3
# <char> <int> <int> <int>
# 1:      b     1     7    13
# 2:      b     2     8    14
# 3:      b     3     9    15
# 4:      a     4    10    16
# 5:      a     5    11    17
# 6:      c     6    12    18

DT[,
   .(val = c(var_1,var_2)),
   by = ID]

# ID   val
# <char> <int>
# 1:      b     1
# 2:      b     2
# 3:      b     3
# 4:      b     7
# 5:      b     8
# 6:      b     9
# 7:      a     4
# 8:      a     5
# 9:      a    10
# 10:      a    11
# 11:      c     6
# 12:      c    12

### Have all vals of col var_1 and var_2 concatenated, but returned as a list column ----

DT[,
   .(val = list(c(var_1,var_2))),
     by = ID]

# ID         val
# <char>      <list>
#   1:      b 1,2,3,7,8,9
# 2:      a  4, 5,10,11
# 3:      c        6,12

# 1) Concatenate teh values with c(var_1,var_2) for each group, wrap that with list() so for each group, we return a list of all concatenated values
# 2) Commas are for display only, a list column can contain any object in each cell. Here each cell is a vector, and some cells have longer vectors than others

# Play around with the usage in j

## (1) look at the difference between
DT[, print(c(var_1,var_2)), by = ID]
# [1] 1 2 3 7 8 9
# [1]  4  5 10 11
# [1]  6 12
# Empty data.table (0 rows and 1 cols): ID
# Returns a vector with length 6,4,2 here

## (2) and
DT[, print(list(c(var_1,var_2))), by = ID]
# [[1]]
# [1] 1 2 3 7 8 9
# 
# [[1]]
# [1]  4  5 10 11
# 
# [[1]]
# [1]  6 12
# Empty data.table (0 rows and 1 cols): ID
# Returns a list of length 1 for each group, with first element holding vectors of length 6,4,2
# (1) has length 6+4+2=12, but (2) has length 1+1+1=3

# Reference Semantics ----
# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-reference-semantics.html

input <- if (file.exists("flights14.csv")) {
  
  "flights14.csv"
} else {
  "https://raw.githubusercontent.com/Rdatatable/data.table/master/vignettes/flights14.csv"
}
flights <- fread(input)
flights

# 1) Reference Semantics ----

# Here is how to add new cols, update or delete rather than creating new datasets as before

## a) Background ----

# Here's a data frame

DF = data.frame(ID = c("b","b","b","a","a","c"), var_1 = 1:6, var_2 = 7:12, var_3 = 13:18)
DF
#   ID var_1 var_2 var_3
# 1  b     1     7    13
# 2  b     2     8    14
# 3  b     3     9    15
# 4  a     4    10    16
# 5  a     5    11    17
# 6  c     6    12    18

# If we write
DF$var_3 <- 18:13 #(1) replace entire column

DF$var_3[DF$ID == "b"] <- 15:13 #(2) subassign in column "var_3"

# The := operator makes no copies irrespective of the R version because := updates DT columns in place (by reference)


# These result in deep copies of the DF; or shallow copies
# But anyways, the entire column for (2) is copied, so the more cols one subassigns in the same query, the more deep copies R makes

# Shallow copy: copies the vector of column pointers, not a physical copy of the actual data
# Deep copy: Copies the entire data to another location in memory

## b) The := operator ----

# Used in j in two ways

### a) LHS := RHS form ----

#DT[, c("colA","colB",...) := list(valA, valB, ...)]
# when you have only one column to assign, you can drop the quotes and list() for convenience
#DT[, colA := valA]

# LHS takes a character vector of column names, and RHS a list of VALUES
# RHS just needs to be a list, irrespective of how it's generated (e.g with lapply(), list(), mget(), mapply() etc )
# This is usually easy to program with, esp. when you don't know the cols to assign values to in advance

### b) Functional form ----
# DT[, `:=`(colA = valA, # valA is assigned to colA
#           colB = valB, # valB is assigned to colB
#           ...
# )]
# Handy if you'd like to jot some comments down for later
# The result is returned invisibly

# Because := is available in j, we can combine it with i and by ops just like the aggregation ops we saw earlier
# In the two forms of :=, we don't assign the result back to a variable because we don't need to; the input DT is modified by reference

# 2 Add/update/delete columns by reference ----
## a) Add columns by reference ----
### Add speed and total delay of each fliht to flights DT ----

flights[, `:=`(speed = distance / (air_time/60), # speed in mph(mi/h)
               delay = arr_delay + dep_delay # delay in minutes
               )]

head(flights)

# year month   day dep_delay arr_delay carrier origin   dest air_time distance  hour    speed delay
# <int> <int> <int>     <int>     <int>  <char> <char> <char>    <int>    <int> <int>    <num> <int>
#   1:  2014     1     1        14        13      AA    JFK    LAX      359     2475     9 413.6490    27
# 2:  2014     1     1        -3        13      AA    JFK    LAX      363     2475    11 409.0909    10
# 3:  2014     1     1         2         9      AA    JFK    LAX      351     2475    19 423.0769    11
# 4:  2014     1     1        -8       -26      AA    LGA    PBI      157     1035     7 395.5414   -34
# 5:  2014     1     1         2         1      AA    JFK    LAX      350     2475    13 424.2857     3
# 6:  2014     1     1         4         0      AA    EWR    LAX      339     2454    18 434.3363     4
# 

# Alternatively, using `LHS := RHS` form:
flights[,c("speed","delay") := list(distance/(air_time/60),
                                    arr_delay + dep_delay)]

# Notes:
# We didn't have to assign the result back to flights
# The Flights DT now contains the two newly added columns, that's what "added by reference" means

# The functional form let us have comments on the side to explain what the computation did

## b) Update some rows of columns by reference - sub-assign by reference ----

# Let's look at all the hours available in the flights DT:

# Get all 'hours in flights
flights[, sort(unique(hour))]

# 0:24
# That gives 25 unique values of the data, with BOTH 0 and 24. LEt's replace 24 with 0:

### Replace the rows where hours==24 with the value 0:
# sub-assign by reference
flights[hour == 24L, hour := 0L]

# Can use i along with := in j in the same way as the intro
# Column "hour" is replace with 0 only on the row indices where hours==24: specified in i is TRUE
# := returns the result invisibly. Someties we might want to see the result after assignment, which we can get with

flights[hour == 24L, hour := 0L][]

# Q: What's the difference between
flights[hour==24L, hour := 0L]

# and 
flights[hour==24L][, hour := 0L]

#? A: the latter creates a new DT; the first just replaces the DF in reference

## c) Delete column by reference ----

### Remove the "delay" column ----

flights[, 
        c("delay") := NULL]

head(flights)


# Or with the funcitonal form

flights[,
        `:=`(delay = NULL)]

# Delete convenience: assigning NULL to a column DELETES that column instantly
# Can also pass col numbers rather than names to LHS but it's good programming practice to use the col names
# When there's just one col to delete, can drop the c() and double quotes and just use the colname unquoted:
flights[,
        delay := NULL]

## d) := along with grouping using by
### Add a new column which contains for each origin-dest pair the max speed

flights[,
        max_speed := max(speed),
        by = .(origin,dest)]

head(flights)

# year month   day dep_delay arr_delay carrier origin   dest air_time
# <int> <int> <int>     <int>     <int>  <char> <char> <char>    <int>
#   1:  2014     1     1        14        13      AA    JFK    LAX      359
# 2:  2014     1     1        -3        13      AA    JFK    LAX      363
# 3:  2014     1     1         2         9      AA    JFK    LAX      351
# 4:  2014     1     1        -8       -26      AA    LGA    PBI      157
# 5:  2014     1     1         2         1      AA    JFK    LAX      350
# 6:  2014     1     1         4         0      AA    EWR    LAX      339
# distance  hour    speed max_speed
# <int> <int>    <num>     <num>
#   1:     2475     9 413.6490  526.5957
# 2:     2475    11 409.0909  526.5957
# 3:     2475    19 423.0769  526.5957
# 4:     1035     7 395.5414  517.5000
# 5:     2475    13 424.2857  526.5957
# 6:     2454    18 434.3363  518.4507

# add the new col max_speed using the := operator by reference
# Provide cols to the group the same way as before. For each group, max(speed) is computed, returning a single value. That value is recycled to fit the length of the group. And no copies are made: flights DT is modified in-place

# Could also have provided the character vector: e.g. c("origin","dest")

## e) Multiple columns and := ----

### Add two more columns computing max() or dep_delay and arr_delay for each month using .SD ----

in_cols = c("dep_delay","arr_delay")
out_cols = c("max_dep_delay","max_arr_delay")
flights[,                                   # for all rows
        c(out_cols) := lapply(.SD, max),    # over each Subset of the Data compute the max
        by = month,                         # create 12 differend SDs, one for each month
        .SDcols = in_cols                   # compute the max only for the in_cols columns
        ]

flights
# year month   day dep_delay arr_delay carrier origin   dest air_time distance  hour    speed max_speed max_dep_delay max_arr_delay
# <int> <int> <int>     <int>     <int>  <char> <char> <char>    <int>    <int> <int>    <num>     <num>         <int>         <int>
#   1:  2014     1     1        14        13      AA    JFK    LAX      359     2475     9 413.6490  526.5957           973           996
# 2:  2014     1     1        -3        13      AA    JFK    LAX      363     2475    11 409.0909  526.5957           973           996
# 3:  2014     1     1         2         9      AA    JFK    LAX      351     2475    19 423.0769  526.5957           973           996
# 4:  2014     1     1        -8       -26      AA    LGA    PBI      157     1035     7 395.5414  517.5000           973           996
# 5:  2014     1     1         2         1      AA    JFK    LAX      350     2475    13 424.2857  526.5957           973           996
# ---                                                                                                                                   
#   253312:  2014    10    31         1       -30      UA    LGA    IAH      201     1416    14 422.6866  508.7425          1498          1494
# 253313:  2014    10    31        -5       -14      UA    EWR    IAH      189     1400     8 444.4444  538.4615          1498          1494
# 253314:  2014    10    31        -8        16      MQ    LGA    RDU       83      431    11 311.5663  445.8621          1498          1494
# 253315:  2014    10    31        -4        15      MQ    LGA    DTW       75      502    11 401.6000  456.3636          1498          1494
# 253316:  2014    10    31        -5         1      MQ    LGA    SDF      110      659     8 359.4545  434.5055          1498          1494
# 

# This uses the LHS:= RHS form, storing input colnames and the new cols to add in separate vars, providing them to .SDcols and for LHS for better readability
# Since we allow assignment by reference without quotign colnames when there's only one column, we can't do "out_cols := lapply(.SD, max). That would result in adding one new column called "out_cols". Instead do either c(out_cols or (out_cols). Wrapping the varname with () s enough to differentiate between the two

# LHS:= RHS form lets us operate on multiple cols: in the RHS, to compute the max on cols specified in .SDcols, make use of the lapply() and .SD in the same way as before, which returns a list of 2 elements: the max val corresponding to dep_delay and arr_delay for each group

# Clean up and delete the newly created cols:

# RHS gets automatically recycled to length of LHS:

flights[,
        c("speed","max_speed","max_dep_delay","max_arr_delay") := NULL]

head(flights)

# everything removed

# 3 := and copy() ----

#:= modifies the input object by reference. Sometimes we might want to use the update by reference feature for its side effect; other times we might not want to change the original object, in which case we can use the copy() function

## a) := for its side effect ----

# Suppose we want a function that returns max speed for each month, AND we want to add the column speed to flights. We could write:

foo <- function(DT){
  DT[, speed := distance / (air_time/60)]
  DT[, 
     .(max_speed = max(speed)),
     by = month]
}

ans = foo(flights)

head(flights)
# year month   day dep_delay arr_delay carrier origin   dest air_time distance  hour    speed
# <int> <int> <int>     <int>     <int>  <char> <char> <char>    <int>    <int> <int>    <num>
#   1:  2014     1     1        14        13      AA    JFK    LAX      359     2475     9 413.6490
# 2:  2014     1     1        -3        13      AA    JFK    LAX      363     2475    11 409.0909
# 3:  2014     1     1         2         9      AA    JFK    LAX      351     2475    19 423.0769
# 4:  2014     1     1        -8       -26      AA    LGA    PBI      157     1035     7 395.5414
# 5:  2014     1     1         2         1      AA    JFK    LAX      350     2475    13 424.2857
# 6:  2014     1     1         4         0      AA    EWR    LAX      339     2454    18 434.3363

ans
# month max_speed
# <int>     <num>
#   1:     1  535.6425
# 2:     2  535.6425
# 3:     3  549.0756
# 4:     4  585.6000
# 5:     5  544.2857
# 6:     6  608.5714
# 7:     7  630.4348
# 8:     8  532.5939
# 9:     9  595.2995
# 10:    10  546.9231

# speed has been added to the flights DT, because := performs operations by reference. Since DT is the function argument and flights refers to the same object in memory, modifying DT also changes flights
# ans then contains the max speed for each month

## b) The copy() function
# Previously we used := for this side effect, but sometimes we might want to pass a DT object to a function and use the := operator without updating the original object. We can do this with copy(): copy() deep copies the input object, so any update by reference operations won't affect the original

# Where is copy() essential?
# 1) Maybe we don't want the input DT to be modified by reference
# First delete the speed column

flights[,
        speed := NULL]

# now use this function

foo <- function(DT){
  DT <- copy(DT)        ## deep copy
  DT[,
     speed := distance / (air_time/60)] ## doesn't affect 'flights'
  DT[,
     .(max_speed = max(speed)),
     by = month]
}

ans <- foo(flights)

head(flights)
# year month   day dep_delay arr_delay carrier origin   dest air_time distance  hour
# <int> <int> <int>     <int>     <int>  <char> <char> <char>    <int>    <int> <int>
#   1:  2014     1     1        14        13      AA    JFK    LAX      359     2475     9
# 2:  2014     1     1        -3        13      AA    JFK    LAX      363     2475    11
# 3:  2014     1     1         2         9      AA    JFK    LAX      351     2475    19
# 4:  2014     1     1        -8       -26      AA    LGA    PBI      157     1035     7
# 5:  2014     1     1         2         1      AA    JFK    LAX      350     2475    13
# 6:  2014     1     1         4         0      AA    EWR    LAX      339     2454    18
ans
# month max_speed
# <int>     <num>
#   1:     1  535.6425
# 2:     2  535.6425
# 3:     3  549.0756
# 4:     4  585.6000
# 5:     5  544.2857
# 6:     6  608.5714
# 7:     7  630.4348
# 8:     8  532.5939
# 9:     9  595.2995
# 10:    10  546.9231

# Using copy() didn't update flights by reference: flights doesn't contain "speed"
# ans contains the max speed by onth

# We could improve this more by shallow rather than deep copying

# When we store the colnames to a variable, e.g. DT_n = names(DT) and then add/update/delete cols by reference, this would modify DT_n unless we did copy(names(DT))

DT = data.table(x = 1L,
                y = 2L)

DT
# x     y
# <int> <int>
#   1:     1     2
DT_n = names(DT)

DT_n
# [1] "x" "y"

## add a new col by reference
DT[,
   z := 3L]

## DT_n also gets updated

DT_n
# [1] "x" "y" "z"

## using copy()

DT_n = copy(names(DT))

DT[, w := 4L]
DT_n
# [1] "x" "y" "z"
# DT_n doesn't get updated

# Keys and Fast Binary Search Based Subset ----

# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-keys-fast-subset.html

# Data:
input <- if (file.exists("flights14.csv")) {
  
  "flights14.csv"
} else {
  "https://raw.githubusercontent.com/Rdatatable/data.table/master/vignettes/flights14.csv"
}
flights <- fread(input)
flights

dim(flights)

# 253316 x 11

# This vignette:
# 1) introduces the concept of a key, and set and uses keys to perform fast binary searhc-based subsets in i
# 2) Combine key-based subsets wtih j and by in the same way as prior
# 3) Look at other useful arguments: mult and nomatch
# 4) Look at the advantage of setting keys for fast binary search-based subsets and compare with the usual vector scan approach

## 1) Keys ----

### a) What is a key? ----

# we can subset rows in i using logical expressions, row numbers and order(). Keys also lets us subset super fast

# First compare to DFs which have a row naes attribute:
set.seed(1L)
DF = data.frame(ID1 = sample(letters[1:2], 10, TRUE),
                ID2 = sample(1:3, 10, TRUE),
                val = sample(10),
                stringsAsFactors = FALSE,
                row.names = sample(LETTERS[1:10]))
DF
#   ID1 ID2 val
# I   a   1  10
# D   a   3   9
# G   a   1   4
# A   a   1   7
# B   a   1   1
# E   b   1   8
# C   b   2   3
# J   b   1   2
# F   b   1   5
# H   a   2   6

rownames(DF)
#  [1] "I" "D" "G" "A" "B" "E" "C" "J" "F" "H"

# we can subset a particular row using its rowname:

DF["C",]
#   ID1 ID2 val
# C   b   2   3

# Row names basically index the rows of a DF, however:
# 1) each row has exactly 1 row name
# -- but a person for example has at least 2 names, a first and second e.g. a phone directory with surname, first name
# 2) Row names should be unique

rownames(DF) = sample(LETTERS[1:5], 10, TRUE)
# Warning: non-unique values when setting 'row.names': 'C', 'D'
# Error in `.rowNamesDF<-`(x, value = value): duplicate 'row.names' are not allowed

# Convert it to a DT

DT = as.data.table(DF)
DT
#        ID1   ID2   val
#     <char> <int> <int>
#  1:      a     1    10
#  2:      a     3     9
#  3:      a     1     4
#  4:      a     1     7
#  5:      a     1     1
#  6:      b     1     8
#  7:      b     2     3
#  8:      b     1     2
#  9:      b     1     5
# 10:      a     2     6

rownames(DT)
#  [1] "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9"  "10"

# Row names have been reset
# DT never uses row names. It inherits DF attributes so has a row names attribute but it doesn't use them
# IF you want to preserve row names, use keep.rowname = TRUE in as.data.table(), and this creates a new col called rn, assigning row names to this col

# DTs instead set and use keys, which is bascially a supercharged rowname

# Keys:
# 1) We can set keys on multiple columns, and columns of different types (except list or complex)
# 2) duplicates are allowed. Since rows are sorted by key, duplicates will be consecutive
# 3) Setting a key does two things:
## -1 physically reorders rows of the DT by the columns provided by reference, in INCREASING order
## -2 marks the columns as key columns by setting an attribute called sorted to the DT
# Since rows are reordered, a DT can have at MOST one key, because it can't be sorted in more than one way

## b) Set, get, use keys on a DT ----
### Set origin as the key in DT flights ----

setkey(flights, origin)

head(flights)
# Key: <origin>
#     year month   day dep_delay arr_delay carrier origin   dest air_time distance  hour
#    <int> <int> <int>     <int>     <int>  <char> <char> <char>    <int>    <int> <int>
# 1:  2014     1     1         4         0      AA    EWR    LAX      339     2454    18
# 2:  2014     1     1        -5       -17      AA    EWR    MIA      161     1085    16
# 3:  2014     1     1       191       185      AA    EWR    DFW      214     1372    16
# 4:  2014     1     1        -1        -2      AA    EWR    DFW      214     1372    14
# 5:  2014     1     1        -3       -10      AA    EWR    MIA      154     1085     6
# 6:  2014     1     1         4       -17      AA    EWR    DFW      215     1372     9

## alternatively we can provide character vectors to the function 'setkeyv()'

setkeyv(flights, "origin") # useful to program with

# setkey() lets you provide colnames without quotes, helpful interactively
# setkeyv() lets you pass a character vec of colnames, useful with functions
# Note: didn't have to assign the result back to a variable because setkey(), setkeyv() and := all modify the input DT by reference, returning the result invisibly
# the DT is now reordered by origin, the col provided
# This uses very little memory

### set* and := ----
# set* functions and := are the only ones which modify by reference
# Once you key the DT by certain coumns, you can subset by querying those key cols in the .() notation (i.e. an alias to list())

### use key col origin to subset all rows with origin airport JFK ----

flights[.("JFK")]
flights[.("JFK")]
# Key: <origin>
#         year month   day dep_delay arr_delay carrier origin   dest air_time distance  hour
#        <int> <int> <int>     <int>     <int>  <char> <char> <char>    <int>    <int> <int>
#     1:  2014     1     1        14        13      AA    JFK    LAX      359     2475     9
#     2:  2014     1     1        -3        13      AA    JFK    LAX      363     2475    11
#     3:  2014     1     1         2         9      AA    JFK    LAX      351     2475    19
#     4:  2014     1     1         2         1      AA    JFK    LAX      350     2475    13
#     5:  2014     1     1        -2       -18      AA    JFK    LAX      338     2475    21
#    ---                                                                                    
# 81479:  2014    10    31        -4       -21      UA    JFK    SFO      337     2586    17
# 81480:  2014    10    31        -2       -37      UA    JFK    SFO      344     2586    18
# 81481:  2014    10    31         0       -33      UA    JFK    LAX      320     2475    17
# 81482:  2014    10    31        -6       -38      UA    JFK    SFO      343     2586     9
# 81483:  2014    10    31        -6       -38      UA    JFK    LAX      323     2475    11

## alternatively
flights[J("JFK")] #(or) 
flights[list("JFK")]

# J() is a direct alias of list()
# Because the key column is origin, the value of "JFK" is sufficient here
# The .() clarifies that the task requires looking up the value JFK in the key col of the DT
# Row indices corresponding to JFK are obtained first; because there's no expression in j, all cols with those rows are returned
# With a single column key in character type, you can drop the .() and use the values directly like a rowname subset in a DF
flights["JFK"] ## == flights[.("JFK")]

# We can subset any amount of values
flights[c("JFK","LGA")] # == flights[.(c("JFK","LGA"))]

### Return the cols that a DT is keyed by ----

key(flights)
# "origin"
# returns a character vec of all key cols, if no key set then returns NULL

## c) Keys and multiple columns ----

### Set keys on both origin and dest columns ----

setkey(flights, origin, dest)

head(flights)
# Key: <origin, dest>
#     year month   day dep_delay arr_delay carrier origin   dest air_time distance  hour
#    <int> <int> <int>     <int>     <int>  <char> <char> <char>    <int>    <int> <int>
# 1:  2014     1     2        -2       -25      EV    EWR    ALB       30      143     7
# 2:  2014     1     3        88        79      EV    EWR    ALB       29      143    23
# 3:  2014     1     4       220       211      EV    EWR    ALB       32      143    15
# 4:  2014     1     4        35        19      EV    EWR    ALB       32      143     7
# 5:  2014     1     5        47        42      EV    EWR    ALB       26      143     8
# 6:  2014     1     5        66        62      EV    EWR    ALB       31      143    23

## or alternatively
setkeyv(flights, c("origin", "dest")) # provide a character vector of column names

key(flights)
# [1] "origin" "dest"

# Sorts the DT first by origin, then by dest by reference

### Subset all rows with key cols where first key col origin matches JFK and second dest matches MIA ----

flights[.("JFK","MIA")]
# Key: <origin, dest>
#        year month   day dep_delay arr_delay carrier origin   dest air_time distance  hour
#       <int> <int> <int>     <int>     <int>  <char> <char> <char>    <int>    <int> <int>
#    1:  2014     1     1        -1       -17      AA    JFK    MIA      161     1089    15
#    2:  2014     1     1         7        -8      AA    JFK    MIA      166     1089     9
#    3:  2014     1     1         2        -1      AA    JFK    MIA      164     1089    12
#    4:  2014     1     1         6         3      AA    JFK    MIA      157     1089     5
#    5:  2014     1     1         6       -12      AA    JFK    MIA      154     1089    17
#   ---                                                                                    
# 2746:  2014    10    31        -1       -22      AA    JFK    MIA      148     1089    16
# 2747:  2014    10    31        -3       -20      AA    JFK    MIA      146     1089     8
# 2748:  2014    10    31         2       -17      AA    JFK    MIA      150     1089     6
# 2749:  2014    10    31        -3       -12      AA    JFK    MIA      150     1089     5
# 2750:  2014    10    31        29         4      AA    JFK    MIA      146     1089    19

# JFK first matches to origin, within those matching rows MIA is matched against dest. Because there's no j provided, all cols corresponding to those row indices are returned

### Subset all rows where just first key col matches JFK ----

key(flights)

flights[.("JFK")]## or in this case simply flights["JFK"], for convenience
# Key: <origin, dest>
#         year month   day dep_delay arr_delay carrier origin   dest air_time distance  hour
#        <int> <int> <int>     <int>     <int>  <char> <char> <char>    <int>    <int> <int>
#     1:  2014     1     1        10         4      B6    JFK    ABQ      280     1826    20
#     2:  2014     1     2       134       161      B6    JFK    ABQ      252     1826    22
#     3:  2014     1     7         6         6      B6    JFK    ABQ      269     1826    20
#     4:  2014     1     8        15       -15      B6    JFK    ABQ      259     1826    20
#     5:  2014     1     9        45        32      B6    JFK    ABQ      267     1826    20
#    ---                                                                                    
# 81479:  2014    10    31         0       -18      DL    JFK    TPA      142     1005     8
# 81480:  2014    10    31         1        -8      B6    JFK    TPA      149     1005    19
# 81481:  2014    10    31        -2       -22      B6    JFK    TPA      145     1005    14
# 81482:  2014    10    31        -8        -5      B6    JFK    TPA      149     1005     9
# 81483:  2014    10    31        -4       -18      B6    JFK    TPA      145     1005     8

### Subset all rows where just second key col dest matches "MIA" ----

flights[.(unique(origin), "MIA")]
# Key: <origin, dest>
#        year month   day dep_delay arr_delay carrier origin   dest air_time distance  hour
#       <int> <int> <int>     <int>     <int>  <char> <char> <char>    <int>    <int> <int>
#    1:  2014     1     1        -5       -17      AA    EWR    MIA      161     1085    16
#    2:  2014     1     1        -3       -10      AA    EWR    MIA      154     1085     6
#    3:  2014     1     1        -5        -8      AA    EWR    MIA      157     1085    11
#    4:  2014     1     1        43        42      UA    EWR    MIA      155     1085    15
#    5:  2014     1     1        60        49      UA    EWR    MIA      162     1085    21
#   ---                                                                                    
# 9924:  2014    10    31       -11        -8      AA    LGA    MIA      157     1096    13
# 9925:  2014    10    31        -5       -11      AA    LGA    MIA      150     1096     9
# 9926:  2014    10    31        -2        10      AA    LGA    MIA      156     1096     6
# 9927:  2014    10    31        -2       -16      AA    LGA    MIA      156     1096    19
# 9928:  2014    10    31         1       -11      US    LGA    MIA      164     1096    15

# MIA has to find the match values in dest on the matching rows provided by the first col, origin
# we can't skip the values of key cols before, so we provide all unique values
# MIA is automatically recycled to fit the length of unique(origin) which is 3

# 2 Combining keys with j and by ----

## a) Select in j ----

### Return arr_Delay as a data table corresponding to origin = LGA, dest = TPA ----

key(flights)

flights[.("LGA","TPA"),.(arr_delay)]
#       arr_delay
#           <int>
#    1:         1
#    2:        14
#    3:       -17
#    4:        -4
#    5:       -12
#   ---          
# 1848:        39
# 1849:       -24
# 1850:       -12
# 1851:        21
# 1852:       -11

# Row indices from origin==LGA, dest==TPA are obtained with a key-based subset
# once we have those row indices, look at j which requires the arr_delay column
# also could do

flights[.("LGA","TPA"),
        "arr_delay",
        with=FALSE]

## b) Chaining ----

### Chain the column in decreasing order ----

flights[.("LGA","TPA"),
        .(arr_delay)][
          order(-arr_delay)
        ]
#       arr_delay
#           <int>
#    1:       486
#    2:       380
#    3:       351
#    4:       318
#    5:       300
#   ---          
# 1848:       -40
# 1849:       -43
# 1850:       -46
# 1851:       -48
# 1852:       -49

## c) Compute or do in j ----

### Find max arrival dleay corresponding to origin==LGA, dest == TPA ----

flights[.("LGA","TPA"),
        max(arr_delay)]
# 486

## d) Sub-assign by reference using := in j ----
# get all 'hours' in flights
flights[,
        sort(unique(hour))]

# 0:25

# replace 24 with 0, this time using key

setkey(flights, hour)
key(flights)

flights[.(24),
        hour := 0L]
key(flights)
# hour # still, vignette says this should be reoved


flights[, sort(unique(hour))]
#  [1]  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23

# 24 removed

## e) Aggregation using by ----

setkey(flights, origin, dest)

### Get max departure delay for each month correspnding to origin == JFK, and order by month:

ans <- flights["JFK",
               max(dep_delay),
               keyby = month]
head(ans)
# Key: <month>
#    month    V1
#    <int> <int>
# 1:     1   881
# 2:     2  1014
# 3:     3   920
# 4:     4  1241
# 5:     5   853
# 6:     6   798
key(ans)
# [1] "month"

# subset key column origin to get the rows with JFK
# then we just need 2 cols: month and dep_delay to get max() for each group
# DT's query just subsets those two cols
# then on that subset group by month and compute max(dep_Delay)
# keyby automatically keys the result by month: it orders and sets month as the key column

# 3 Additional arguments mult and nomatch ----

## a) mult ----

# Choose if "all" matching rows should be returned or just first or last. Default is "all'

### Subset only the first matching rows for origin==JFK and dest == MIA ----

flights[.("JFK","MIA"),
        mult = "first"]

# Key: <origin, dest>
#     year month   day dep_delay arr_delay carrier origin   dest air_time distance  hour
#    <int> <int> <int>     <int>     <int>  <char> <char> <char>    <int>    <int> <int>
# 1:  2014     1     1         6         3      AA    JFK    MIA      157     1089     5

### Subset last matching row where origin == JFK LGA or EWR and dest == XNA ----

flights[.(c("LGA", "JFK", "EWR"), "XNA"), mult = "last"]
#     year month   day dep_delay arr_delay carrier origin   dest air_time distance  hour
#    <int> <int> <int>     <int>     <int>  <char> <char> <char>    <int>    <int> <int>
# 1:  2014     5    23       163       148      MQ    LGA    XNA      158     1147    18
# 2:    NA    NA    NA        NA        NA    <NA>    JFK    XNA       NA       NA    NA
# 3:  2014     2     3       231       268      EV    EWR    XNA      184     1131    12

# Note JFK XNA doesn't have any matches so returns NA
# the XNA is recycled to fit length 3 for the first key

## b) nomatch ----

# Choose if unmatching queries should return NA or skip 

### Subset all rows only if there's a match ----

flights[.(c("LGA","JFK","EWR"),"XNA"),
        mult = "last",
        nomatch = NULL]
#     year month   day dep_delay arr_delay carrier origin   dest air_time distance  hour
#    <int> <int> <int>     <int>     <int>  <char> <char> <char>    <int>    <int> <int>
# 1:  2014     5    23       163       148      MQ    LGA    XNA      158     1147    18
# 2:  2014     2     3       231       268      EV    EWR    XNA      184     1131    12

# the default for nomatch is NA; set to NULL to skip queries with no matches

# 4 Binary search vs. vector scans ----

# What's the advantage of:

# key by origin, dest columns
flights[.("JFK","MIA")]

flights[origin == "JFK" & dest == "MIA"]

# binary search subsets are super fast

# compare with slower vector scan

setkey(flights, NULL)

flights[origin == "JFK" & dest == "MIA"]

# Way faster to use keys than regular vector scan, use those

# Secondary indices and auto indexing ----

# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-secondary-indices-and-auto-indexing.html

# Data:
input <- if (file.exists("flights14.csv")) {
  
  "flights14.csv"
} else {
  "https://raw.githubusercontent.com/Rdatatable/data.table/master/vignettes/flights14.csv"
}
flights <- fread(input)
flights

dim(flights)

# 253316 x 11

# Goals:
# 1) Discuss secondary indices, and why we need them (rather than setting keys)
# 2) Perform fast subsetting with the "on" argument, which computes secondary indices internally for the tasks (temporarily) and reuses if one already exists
# 3) Auto index: which creates secondary indices automatically on native R syntax

# 1 Secondary Indices ----
## a) What are Secondary Indices? ----
# Secondary indices: similar to keys in DT except:
# 1) Doesn't physically reorder the entire DT in RAM, it just gets the order for the set of cols provided and stores the order vector in an attribute called "index"
# 2) You can have multiple secondary indices for a DT

## b) Set and get secondary indices ----
### Set origin as a secondary index in flights ----

setindex(flights,origin)
head(flights)
#     year month   day dep_delay arr_delay carrier origin   dest air_time distance  hour
#    <int> <int> <int>     <int>     <int>  <char> <char> <char>    <int>    <int> <int>
# 1:  2014     1     1        14        13      AA    JFK    LAX      359     2475     9
# 2:  2014     1     1        -3        13      AA    JFK    LAX      363     2475    11
# 3:  2014     1     1         2         9      AA    JFK    LAX      351     2475    19
# 4:  2014     1     1        -8       -26      AA    LGA    PBI      157     1035     7
# 5:  2014     1     1         2         1      AA    JFK    LAX      350     2475    13
# 6:  2014     1     1         4         0      AA    EWR    LAX      339     2454    18

## alternatively we can provide character vectors to the function 'setindexv()'
setindexv(flights, "origin") # useful to program with

# 'index' attribute added
names(attributes(flights))
# [1] "names"             "row.names"         "class"             ".internal.selfref"
# [5] "index"

# Note "flights" is NOT reordered in increasing order of origin as would have been with setkey()
# index attribute has been added

setindex(flights, NULL) # removes secondary indices

names(attributes(flights))
# [1] "names"             "row.names"         "class"             ".internal.selfref"

### Obtain all secondary indices set so far ----

setindex(flights, origin)

indices(flights)
# origin

setindex(flights, origin, dest)
indices(flights)
# [1] "origin"       "origin__dest"

# by creating another index on the cols origin,dest we don't lost the first index created on "origin" ie we can have multiple secondary indices

# c) Why use secondary indices ? ----
### Reordering a DT can be expensive and not ideal ---

# suppose we want a fast key-based subset on origin for the value JFK:

setkey(flights,origin)
flights["JFK"] # or flights[.("JFK")]

#setkey() requires:
# a) computing the order vector for the column(s), here origin
# b) reordering the entire DT by reference based on the order vector cmputed

# Computing the order isn't the long thing since DT sorts that fast
# But reordering the DT could be time consuming 

# unless we want to repeatedly subset on the same column, the fast key-based subsetting could be nullified by the time to reorder dependning on the DT dimensions

### There's only one key, max ----

# If we wanted to repeat the same op but on dest col, we would need to do

setkey(flights,dest)
flights["LAX"]

# this would reorder flights by dest, again
# we would like the fast subsetting without reordering, that's what secondary indices does

### Secondary indices can be reused ----

# There can be multiple secondary indices and this just requires storing the order vector as an attribute, so you don't have to recompute the order vector if you made it once

### The on argument allows for cleaner syntax

## on:
# subset by computing secondary indices on the fly rather than setindex() every time
# ruse existing indices just by checking attributes
# have the cols on which the subset is performed as part of the syntax so you can follow the code easier
# on argument can be used for keyed subsets which can be helpful for readability 

# 2 Fast subsetting with on and secondary indices ----

## a) Fast subset in i ----

### Subset all rows where origin airport matches "JFK" with on:

flights["JFK",
        on = "origin"]

#         year month   day dep_delay arr_delay carrier origin   dest air_time distance  hour
#        <int> <int> <int>     <int>     <int>  <char> <char> <char>    <int>    <int> <int>
#     1:  2014     1     1        14        13      AA    JFK    LAX      359     2475     9
#     2:  2014     1     1        -3        13      AA    JFK    LAX      363     2475    11
#     3:  2014     1     1         2         9      AA    JFK    LAX      351     2475    19
#     4:  2014     1     1         2         1      AA    JFK    LAX      350     2475    13
#     5:  2014     1     1        -2       -18      AA    JFK    LAX      338     2475    21
#    ---                                                                                    
# 81479:  2014    10    31        -4       -21      UA    JFK    SFO      337     2586    17
# 81480:  2014    10    31        -2       -37      UA    JFK    SFO      344     2586    18
# 81481:  2014    10    31         0       -33      UA    JFK    LAX      320     2475    17
# 81482:  2014    10    31        -6       -38      UA    JFK    SFO      343     2586     9
# 81483:  2014    10    31        -6       -38      UA    JFK    LAX      323     2475    11

## alternatively
# flights[.("JFK"), on = "origin"] (or)
# flights[list("JFK"), on = "origin"]

# Get fast binary search-based subset, computes index on the fly (but doesn't save it automatically)
# if we'd created the secondary index with setindex() this would reuse rather than (re)computing it
# set verbose = TRUE to see this

setindex(flights,origin)

flights[list("JFK"),
        on = "origin",
        verbose = TRUE][1:5]
# i.V1 has same type (character) as x.origin. No coercion needed.
# on= matches existing index, using index
# Starting bmerge ...
# forder.c received 1 rows and 1 columns
# bmerge done in 0.000s elapsed (0.000s cpu)
# Constructing irows for '!byjoin || nqbyjoin' ... 0.000s elapsed (0.000s cpu)
#     year month   day dep_delay arr_delay carrier origin   dest air_time distance  hour
#    <int> <int> <int>     <int>     <int>  <char> <char> <char>    <int>    <int> <int>
# 1:  2014     1     1        14        13      AA    JFK    LAX      359     2475     9
# 2:  2014     1     1        -3        13      AA    JFK    LAX      363     2475    11
# 3:  2014     1     1         2         9      AA    JFK    LAX      351     2475    19
# 4:  2014     1     1         2         1      AA    JFK    LAX      350     2475    13
# 5:  2014     1     1        -2       -18      AA    JFK    LAX      338     2475    21

### Subset based on origin and dest cols:

flights[.("JFK","LAX"),
        on = c("origin","dest")][1:5]
#     year month   day dep_delay arr_delay carrier origin   dest air_time distance  hour
#    <int> <int> <int>     <int>     <int>  <char> <char> <char>    <int>    <int> <int>
# 1:  2014     1     1        14        13      AA    JFK    LAX      359     2475     9
# 2:  2014     1     1        -3        13      AA    JFK    LAX      363     2475    11
# 3:  2014     1     1         2         9      AA    JFK    LAX      351     2475    19
# 4:  2014     1     1         2         1      AA    JFK    LAX      350     2475    13
# 5:  2014     1     1        -2       -18      AA    JFK    LAX      338     2475    21

# on accepts a character vec of colnames corresponding to the order of the i argument
# because the time to compute the secondary index is small, we don't have to use setindex() unless we want repeated subsetting on the same column

## b) Select in j----

# Now instead of setting keys, use the on argument

### Return arr_Delay column alone as a DT with origin == LGA, dest == TPA ----

flights[.("LGA","TPA"),
        .(arr_delay),
        on = c("origin","dest")]
#       arr_delay
#           <int>
#    1:         1
#    2:        14
#    3:       -17
#    4:        -4
#    5:       -12
#   ---          
# 1848:        39
# 1849:       -24
# 1850:       -12
# 1851:        21
# 1852:       -11

## c) Chaining ----

### Obtain the result in decreasing order 

flights[.("LGA", "TPA"), .(arr_delay), on = c("origin", "dest")][order(-arr_delay)]
#       arr_delay
#           <int>
#    1:       486
#    2:       380
#    3:       351
#    4:       318
#    5:       300
#   ---          
# 1848:       -40
# 1849:       -43
# 1850:       -46
# 1851:       -48
# 1852:       -49

## d) Compute or do in j ----

### Max arrival delay ----
flights[.("LGA", "TPA"), max(arr_delay), on = c("origin", "dest")]
# [1] 486

## e) Sub-assign by reference with := in j ----

# with hours in flights 
# get all 'hours' in flights
flights[, sort(unique(hour))]
#  [1]  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24

# 25 unique value, replace 24 with 0 usng on instead of setting keys

flights[.(24L), hour := 0L, on = "hour"]

# check if 24 is replaced with 0 in the hour column 

flights[,
        sort(unique(hour))]
#  [1]  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23

# this is faster if we just wanted to update a few rows of hour, rather than reordering the entire DT with setkey()

## f) Aggregation using by ----

### get max departure delay for each month corresponding to origin= "JFK", order result by month ----

ans <- flights["JFK",
               max(dep_delay),
               keyby = month,
               on = "origin"]

head(ans)
# Key: <month>
#    month    V1
#    <int> <int>
# 1:     1   881
# 2:     2  1014
# 3:     3   920
# 4:     4  1241
# 5:     5   853
# 6:     6   798

# would've had to set key to orign,dest again if we didn't use on which internally builds secondary indices on the fly

## g) The mult argument ----

# works exactly the same, deafult is "all" by can do first or last:

### Subset only the first matching row where dest matches BOS and DAY ----

flights[c("BOS","DAY"),
        on = "dest",
        mult = "first"]

#     year month   day dep_delay arr_delay carrier origin   dest air_time distance  hour
#    <int> <int> <int>     <int>     <int>  <char> <char> <char>    <int>    <int> <int>
# 1:  2014     1     1         3         1      AA    JFK    BOS       39      187    12
# 2:  2014     1     1        25        35      EV    EWR    DAY      102      533    17

### Subset only the last matching row where origin matches LGA JFK EWR and dest is SNA

flights[.(c("LGA","JFK","EWR"),"XNA"),
        on = c("origin","dest"),
        mult = "last"]

flights[.(c("LGA", "JFK", "EWR"), "XNA"), on = c("origin", "dest"), mult = "last"]
#     year month   day dep_delay arr_delay carrier origin   dest air_time distance  hour
#    <int> <int> <int>     <int>     <int>  <char> <char> <char>    <int>    <int> <int>
# 1:  2014    10    31        -5       -11      MQ    LGA    XNA      165     1147     6
# 2:    NA    NA    NA        NA        NA    <NA>    JFK    XNA       NA       NA    NA
# 3:  2014    10    31        -2       -25      EV    EWR    XNA      160     1131     6

## h) The nomatch argument ----

# Choose if queries that don't match should return NA or get skipped 

### subset all rows only if there's a match ----

flights[.(c("LGA","JFK","EWR"),"XNA"),
        on = c("origin","dest"),
        mult = "last",
        nomatch = NULL]
#     year month   day dep_delay arr_delay carrier origin   dest air_time distance  hour
#    <int> <int> <int>     <int>     <int>  <char> <char> <char>    <int>    <int> <int>
# 1:  2014    10    31        -5       -11      MQ    LGA    XNA      165     1147     6
# 2:  2014    10    31        -2       -25      EV    EWR    XNA      160     1131     6

# 3 Auto Indexing ----

# Currently only operated for == and %in%: an index is automatically created and saved as an attribute
# unlike "on" which cmoputes the index on the fly each time unless it already exists, a secondary index is created

# createa big DT 
set.seed(1L)
dt = data.table(x = sample(1e5L, 1e7L, TRUE), y = runif(100L))
print(object.size(dt), units = "Mb")
# 114.4 Mb

# When we use == r %in% the first time, a secondary index is created and used for the subset 

## have a look at all the attribute names
names(attributes(dt))
# [1] "names"             "row.names"         "class"             ".internal.selfref"

## run thefirst time
(t1 <- system.time(ans <- dt[x == 989L]))
# user  system elapsed 
# 0.07    0.00    0.11
head(ans)
#        x         y
#    <int>     <num>
# 1:   989 0.7757157
# 2:   989 0.6813302
# 3:   989 0.2815894
# 4:   989 0.4954259
# 5:   989 0.7885886
# 6:   989 0.5547504

## secondary index is created
names(attributes(dt))
# [1] "names"             "row.names"         "class"             ".internal.selfref"
# [5] "index"

indices(dt)
# [1] "x"

# The first time you have to create the index and then subset
# Creating a secondayr index only involves creating the order vector, this is faster than vector scans in many cases, but the real advantage comes from later subsets

(t2 <- system.time(dt[x == 989L]))
# user  system elapsed 
# 0.00    0.00    0.03

system.time(dt[x %in% 1989:2012])
#    user  system elapsed 
