# import functions, partner dataset omitted, because partner data impact the purchase activities only

require(plyr)
require(data.table)

rm(list=ls())

paths = list(data='D:/Projects/mhs/dataset/final/', code='D:/Projects/mhs/code/final/')

import_category <- function(){
  cls <- c('integer', 'character', 'integer', 'character')
  categorypr <- read.csv(paste0(paths$data, 'categorypr_pub.csv'), colClasses=cls, nrows=63, sep=",", comment.char = '')[,1:2]
}

#import_partner <- function(){
#  cls <- c('integer','character')
#  partner <- read.csv(paste0(paths$data, 'partner_pub.csv'), colClasses=cls, nrows=3425, sep=",", comment.char = '')
#}

import_product <- function(){
  cls <- c('integer','NULL','NULL','NULL','NULL','NULL','NULL','NULL',
           'character','character', 'NULL','NULL','NULL','NULL','NULL','NULL','NULL','NULL','NULL','NULL','NULL','NULL')
  product <- read.csv(paste0(paths$data, 'product_pub.csv'), colClasses=cls, nrows=12980, sep=",", comment.char = '')
}

import_product_category <- function(){
  cls <- c('integer','integer','NULL')
  productCategory <- read.csv(paste0(paths$data, 'productcategorypr_pub.csv'), colClasses=cls, nrows=13194, sep=",", comment.char = '')
}


import_sales_purchases <- function(){
  cls <- c('NULL','integer','character','NULL','numeric','numeric','numeric','NULL','NULL','numeric','NULL','NULL','numeric',
           'NULL','NULL','numeric','NULL','NULL','integer','integer','factor','NULL','NULL','NULL','NULL','factor','NULL')
  cln <- c('id','date','quantity','')
  sales_purchases <- read.csv(paste0(paths$data, 'salespurchases.csv'), colClasses=cls, nrows=187569, sep=",", comment.char = '')
}

import_warehouse <- function(){
  cls <- c('integer','character')
  warehouser <- read.csv(paste0(paths$data, 'warehouse_pub.csv'), colClasses=cls, nrows=20, sep=",", comment.char = '')
}

