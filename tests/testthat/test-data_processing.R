test_that("aplicar_filtros works correctly", {
  df <- data.frame(
    Ano = c(2023, 2024, 2024, 2025),
    Region = c("CENTRAL", "CENTRAL", "NORTE", "SUR"),
    Especie = c("BOVINO", "BOVINO", "PORCINO", "BOVINO"),
    Casos = c("NUEVOS", "ACUMULADOS", "NUEVOS", "NUEVOS")
  )
  
  # Test year filter
  filtered <- aplicar_filtros(df, anio = "2024")
  expect_equal(nrow(filtered), 2)
  
  # Test region filter
  filtered <- aplicar_filtros(df, region = "CENTRAL")
  expect_equal(nrow(filtered), 2)
  
  # Test multiple filters
  filtered <- aplicar_filtros(df, anio = "2024", region = "CENTRAL")
  expect_equal(nrow(filtered), 1)
  
  # Test no filter
  filtered <- aplicar_filtros(df)
  expect_equal(nrow(filtered), 4)
})
