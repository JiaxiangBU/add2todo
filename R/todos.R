#' Todo List
#'
#' This function helps users to summarise the todo-list in the different documents.

#' @return data.frame
#' @author Jiaxiang Li
#'
#' @importFrom rebus %R%
#' @import rebus
#' @importfrom xfun read_utf8
#' @import stringr
#' @import dplyr
#' @export

todos <- function(dir="."){

    all_list <- list.files(path = dir,full.names = T,recursive = T,pattern = "\\.Rmd$")
    is_todo <- function(path,todo){
        xfun::read_utf8(path) %>%
            stringr::str_subset(todo) %>%
            data.frame(x = .)
        }
    todo <-
        rebus::or(
            rebus::START %R%
                rebus::or(
                    "todo"
                    ,"周" %R% rebus::or("一","二","三","四","五","六","天","日")
                    ,"[12]月\\d+日"
                    ,"明[天日]"
                )
            ,"checkbox1"
        )
    todo_list <-
        dplyr::data_frame(path = all_list) %>%
        dplyr::mutate(
            value =
                purrr::map(
                    path
                    ,is_todo
                    ,todo = todo
                )
        ) %>%
        tidyr::unnest(value) %>%
        rename(value = x) %>%
        dplyr::mutate(value = value %>% stringr::str_remove_all('<input type="checkbox" id="checkbox1" class="styled">')) %>%
        dplyr::filter(!stringr::str_detect(value,"^> ")) %>%
        dplyr::filter(!stringr::str_detect(value,"^明天\\p{Han}{1,3}$")) %>%
        dplyr::filter(!stringr::str_detect(value,"二二")) %>%
        dplyr::filter(!stringr::str_detect(value,"todo_list")) %>%
        dplyr::filter(!stringr::str_detect(value,"todo <-")) %>%
        dplyr::filter(!stringr::str_detect(path,"archive")) %>%
        dplyr::filter(!stringr::str_detect(path,"todo.Rmd")) %>%
        dplyr::arrange(value,path)
    return(todo_list)
}

