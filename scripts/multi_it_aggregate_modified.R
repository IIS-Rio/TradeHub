#' @title Title...
#' @name multi_it_aggregate
#'
#' @description description...
#'
#' @param cfg A json with configuration data ??
#' @param x_vals .
#' @param verbose Logical
#' @param cumulative Logical
#'
#' @details Details.
#'
#' @return data.frame containing ...
#'
#' @export

multi_it_aggregate_modified = function(cfg, x_vals,
                              verbose = T,
                              cumulative = F){
  # Required parameters --------------------------------------------------------
  #HFI = aux_load_raster(raster_path = paste0(cfg$dir$var_dir, cfg$variables$calc_it$hfi), na_fill = "none")

  gamma = cfg$variables$calc_it$gamma
  beta = cfg$variables$calc_it$beta
  z = cfg$variables$calc_it$z

  dg = cfg$variables$calc_it$delta_gamma
  db = cfg$variables$calc_it$delta_beta
  dz = cfg$variables$calc_it$delta_z

  #base_ras = aux_load(paste0(cfg$dir$in_dir, "base_ras"))
  #master_index = aux_load(paste0(cfg$dir$in_dir, "master_index"))

  res = xres(HFI) / 1e3 # raster´s resolution in km
  rad = cfg$variables$calc_it$n_pixels * res # radius considered in move window
  foc_size = (cfg$variables$calc_it$n_pixels * 2) + 1
  foc_index = trunc((foc_size + 1)/2)
  foc_dist = foc_index - 1

  # Creating prop_convert_weight
  #prop_convert = aux_load(paste0(cfg$dir$in_dir, "harmonize_pa"))$prop_convert
  prop_convert = aux_load(paste0(cfg$io$base_path, cfg$io$processed_relative_path, 'harmonize_pa'))$prop_convert
  prop_convert_weight = Map("*", as.list(cfg$variables$calc_it$hfi_weights), as.list(as.data.frame(prop_convert)))
  names(prop_convert_weight) = names(as.list(as.data.frame(prop_convert)))
  prop_convert_weight = Reduce("+", prop_convert_weight)
  # Removing possible negative values :O
  HFI[HFI < 0] = 0

  # propagation of uncertainty
  it_ptr = cfg$variables$variable_names %in% cfg$variables$calc_it$it_variable_name
  quad_unc = cfg$uncertainties$quadratic_propagation[it_ptr]

  # Intactness value -----------------------------------------------

  # Calculating initial intactness value
  it_zero = multi_calc_it_mod(cfg = cfg, base_ras = base_ras, master_index = m_i,
                          prop_convert = prop_convert,dg = 0, db = 0, dz = 0,
                          verbose = F, HFI = HFI, aggregate = T)

  # Calculating final inctactness value
  it_f = multi_calc_it_mod(cfg = cfg, base_ras = base_ras, master_index = m_i,prop_convert = prop_convert,x_vals = x_vals,dg = 0, db = 0, dz = 0,
                       verbose = F, HFI = HFI, aggregate = T)

  # Intactness uncertainty -----------------------------------------------------

  # # Calculating initial intactness uncertainty
  # it_zero_unc =  multi_calc_it_mod(cfg = cfg, base_ras = base_ras, master_index = m_i,prop_convert = prop_convert,dg = dg, db = db, dz = dz,
  #                              verbose = F, HFI = HFI) # aqui talvez não precise de um aggregate = T?
  #
  # it_f_unc =  multi_calc_it_mod(cfg = cfg, base_ras = base_ras, master_index = m_i,prop_convert = prop_convert,
  #                           dg = dg, db = db, dz = dz,
  #                           x_vals = x_vals,
  #                           verbose = F, HFI = HFI) # aqui talvez não precise de um


  # transformar res em raster novamente. Acho q o master index serve

  raster_resultado = 0* base_ras
  raster_resultado[m_i] = it_f


  return(raster_resultado)


}
