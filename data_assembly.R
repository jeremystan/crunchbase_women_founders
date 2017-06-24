#' Extract all data from a specific Crunchbase table
get_cb_data <- function(tbl, con, limit = NULL) {
  sql <- "select * from %s" %>% sprintf(tbl)
  if (!is.null(limit))
    sql <- "%s limit %s" %>% sprintf(sql, limit)
  df <- suppressWarnings(dbGetQuery(con, sql))
  df %>% tbl_df
}

#' Assemble founders data
assemble_founders_data <- function(relationships, people, objects) {

  # 'Founder' or 'founder' in title
  founders <- relationships %>%
    select(person_object_id, relationship_object_id, title, end_at) %>%
    filter(!is.na(str_match(title, '[fF]ounder')))

  # Ensure unique by picking most recent record by end_at
  founders <- founders %>%
    mutate(end_at = as.Date(end_at)) %>%
    group_by(person_object_id) %>%
    arrange(desc(end_at)) %>%
    slice(1) %>%
    ungroup %>%
    select(-end_at)

  # Add founder name and company data
  founders <- founders %>%
    left_join(people %>% select(object_id, first_name),
              by = c("person_object_id" = "object_id")) %>%
    left_join(objects %>% filter(entity_type == 'Company') %>%
                select(id, company = name, region, founded_at, tag_list),
              by = c("relationship_object_id" = "id"))

  # Compute gender data for all first names
  genders <- gender(founders$first_name %>% unique)

  # Add gender data back to founders
  founders <- founders %>%
    left_join(genders %>% select(name, gender, proportion_female),
              by = c("first_name" = "name"))

  founders %>%
    rename(person_id = person_object_id,
           company_id = relationship_object_id)

}

#' Assemble investment rounds data
assemble_rounds_data <- function(funding_rounds, investments, objects,
                                 years, round_codes) {

  # Extract investors
  investors <- objects %>% filter(entity_type == 'FinancialOrg') %>%
    select(investor_id = id, investor = name)

  # Join funding to investments and investors
  rounds <- funding_rounds %>%
    transmute(funding_round_id,
              company_id = object_id,
              funding_date = as.Date(funded_at),
              funding_year = year(as.Date(funded_at)),
              funding_round_code) %>%
    inner_join(investments %>%
                 select(funding_round_id, investor_id = investor_object_id),
               by = "funding_round_id") %>%
    inner_join(investors, by = "investor_id")

  # Filter for specific years and funding round codes
  rounds <- rounds %>%
    filter(funding_year %in% years) %>%
    filter(funding_round_code %in% round_codes) %>%
    mutate(funding_round_code =
             factor(funding_round_code, levels = round_codes))

  # Remove duplicate investments in funding rounds by taking only the earliest
  rounds <- rounds %>%
    group_by(company_id, investor_id, funding_round_code) %>%
    arrange(funding_date) %>%
    slice(1) %>%
    ungroup

  rounds

}

#' Summarize gender
summarize_gender <- function(df) {
  df %>%
    filter(!is.na(gender)) %>%
    summarize(
      num_companies = n_distinct(company_id),
      num_founders = n(),
      pct_female = mean(gender == 'female') * 100
    )
}
