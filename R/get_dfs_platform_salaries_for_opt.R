#' @title DFS Platform Salary Data
#'
#' @description function to pull DFS Platform Salary data
#'
#'
#' @param con RPostgres connection object pointed at the database.
#' @param dfs_platform DFS platform string: DraftKings, FandDuel, Yahoo!
#' @param platforms_list list of DFS platform attributes: keys
#' @examples
#' get_dfs_platform_salaries_for_opt('DrafKings',
#'  list(DraftKings=list(proj_points='dk_proj_points', salary='dksalary', id='dkid')))
#'
#' @export

get_dfs_platform_salaries <- function(con, dfs_platform, platforms_list){
  res <-  NULL

  ## build out query strings; need max year, max week by platform type. Technically, we should have the same player set by
  ## platform but going to do this just in case

  max_year_q <- ## max year where we have projected points
    paste0('select max(year) as max_year from dfs_platform_salaries where ',
           platforms_list[[dfs_platform]][['proj_points']], ' != 0')

  max_year <-
    dbGetQuery(con, max_year_q)

  max_week_q <- ## max week where we have projected points and in the max_year
    paste0('select max(week) as max_week from dfs_platform_salaries where year = ',
           max_year$max_year,
           ' and ',
           platforms_list[[dfs_platform]][['proj_points']], ' != 0')

  max_week <-
    dbGetQuery(con, max_week_q)

  ## build out full query -- select from dfs_platform_salaries where max year, max week, and non-null IDs.
  res_q_str <-
    paste0('select
           year,
           week,
           player,
           position,
           team,
           opponent,',
           platforms_list[[dfs_platform]][['salary']],
           ' as cost,',
           platforms_list[[dfs_platform]][['proj_points']],
           ' as projected_points',
           ' from dfs_platform_salaries where year = ',
           max_year$max_year,
           ' and week = ',
           max_week$max_week,
           ' and ',
           platforms_list[[dfs_platform]][['id']],
           ' is not Null order by projected_points DESC')

  res <-
    dbGetQuery(con, res_q_str) %>% mutate(keep=1)

  return(res)
}
