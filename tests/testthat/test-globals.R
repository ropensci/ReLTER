test_that("Changing deims_base_url with reachable url works",{
  cur<-get_deims_base_url()
  newurl <- "http://www.get-it.it/"
  set_deims_base_url(newurl)
  expect_equal(get_deims_base_url(),newurl)
  set_deims_base_url()  
})

test_that("Changing deims_base_url with unreachable url raises error unless force=TRUE is specified", {
  cur<-get_deims_base_url()
  notreachableurl<-"https://null.get-it.it/"
  expect_error(set_deims_base_url(notreachableurl, force = F))
  expect_warning(set_deims_base_url(notreachableurl, force = T))
  expect_equal(notreachableurl, get_deims_base_url())
  set_deims_base_url()  
})

