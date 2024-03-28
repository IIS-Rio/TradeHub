#' @title Title...
#' @name multi_calc_it
#'
#' @description description...
#'
#' @param cfg A json with configuration data ??
#' @param base_ras list ?
#' @param master_index list ?
#' @param HFI list ?
#' @param prop_convert list ?
#' @param x_vals list ?
#' @param gamma list ?
#' @param beta list ?
#' @param z list ?
#' @param dg list ?
#' @param db list ?
#' @param dz list ?
#' @param verbose logical.
#' @param aggregate .
#'
#' @details Details.
#'
#' @export

multi_calc_it_mod = function(cfg, base_ras, master_index, HFI,
                         prop_convert, x_vals = 0,
                         gamma = -log(0.2)/4, beta = -0.2, z = 0.5,
                         dg = 0, db = 0, dz = 0,
                         verbose = T,
                         aggregate = F,
                         lu_vals = NULL){

  # Creating prop_convert_weight
  if (is.null(prop_convert)) {

    prop_convert_weight = 0

  } else {

    prop_convert_weight = Map("*", as.list(cfg$variables$calc_it$hfi_weights), as.list(as.data.frame(prop_convert)))
    names(prop_convert_weight) = names(as.list(as.data.frame(prop_convert)))
    prop_convert_weight = Reduce("+", prop_convert_weight)

  }

  # Using x_vals as a delta in the pure conservation problem_type
  if ("C" %in% cfg$scenarios$problem_type & length(x_vals) > 1 & !is.null(lu_vals)) {
    nat_vals = Reduce('+', lu_vals[cfg$landscape_features$land_use$class_types == "N"])
    x_vals = x_vals - nat_vals
  }

  # Updating Human Footprint Index if necessary
  HFIreduction = base_ras
  HFIreduction[] = 0
  HFIreduction[m_i] = prop_convert_weight * x_vals
  HFI = HFI - HFIreduction

  # Removing possible negative values
  HFI[HFI < 0] = 0
  # HFI[HFI > 50] = 50

  # Computing a derivative of HFI w.r.t. x assuming linear relation
  dhfi_dx_vals = - prop_convert_weight

  # loading needed objects
  terrestrial_index = aux_load(paste0(cfg$io$base_path, cfg$io$processed_relative_path,"harmonize_lu"))$terrestrial_index

  # Creating matrix with dhfi_dx_vals
  dhfi_dx_terr = rep(0, length(terrestrial_index))
  dhfi_dx_terr[terrestrial_index %in% master_index] = dhfi_dx_vals
  dhfi_dx_ras = aux_plot_vals(dhfi_dx_terr, base_ras = base_ras, master_index = terrestrial_index, plot = F)
  dhfi_dx = matrix(dhfi_dx_ras, nrow = nrow(base_ras), ncol = ncol(base_ras), byrow = TRUE)

  # parameter values defined in Beyer et al intactness paper (suggest you do not change):
  # (chill, changing for uncertainty computation purposes only :D)
  gamma = gamma + dg
  beta = beta + db
  z = z + dz

  # quadratic or linear propagation of uncertainty
  it_ptr = cfg$variables$variable_names %in% cfg$variables$calc_it$it_variable_name
  quad_unc = cfg$uncertainties$quadratic_propagation[it_ptr]

  # create a matrix representing the focal window (with a n_pixels * res radius)
  res = xres(HFI) / 1e3 # raster´s resolution in km
  rad = cfg$variables$calc_it$n_pixels * res # radius considered in move window
  foc_size = (cfg$variables$calc_it$n_pixels * 2) + 1
  foc_index = trunc((foc_size + 1)/2)
  foc.w = matrix(NA, nrow = foc_size, ncol = foc_size)
  for (i in 1:foc_size){
    for (j in 1:foc_size){
      d = sqrt((res * (i - foc_index))^2 + (res * (j - foc_index))^2)
      if (d <= rad) foc.w[i,j] = exp(beta * d)
    }
  }

  # convert that matrix to a vector for subsequent calculations:
  dvec = as.vector(foc.w)

  # Habitat quality (must lie between 0 and 1)
  qm = matrix(exp(-gamma * values(HFI)), nrow = nrow(HFI), ncol = ncol(HFI), byrow = TRUE)

  # delta Q calculation:
  dw = 1e-6 # derivative unit - infinitesimal variation of qm
  delta_qp = matrix(NA, nrow = nrow(HFI), ncol = ncol(HFI))
  dqp_dqm = matrix(NA, nrow = nrow(HFI), ncol = ncol(HFI))
  dqm_dx = matrix(NA, nrow = nrow(HFI), ncol = ncol(HFI))

  foc_dist = foc_index - 1

  # Elapsed time progress bar
  pb = progress::progress_bar$new(
    format = "dQ calculation [:bar] :percent in :elapsed",
    total = (nrow(qm) - foc_index), clear = FALSE, width= 70)

  for (r in foc_index:(nrow(qm) - foc_index)){
    pb$tick()
    for (c in foc_index:(ncol(qm) - foc_index)){
      if (!is.na(qm[r, c])){
        x = as.vector(qm[(r-foc_dist):(r+foc_dist), (c-foc_dist):(c+foc_dist)])

        # Computing aggregated value of Q p
        if (aggregate) {
          delta_qp[r, c] = sum(((qm[r, c]) * x)^z * dvec, na.rm =  TRUE) / sum(dvec[which(!is.na(x))], na.rm = TRUE)
          next
        }

        # Computing derivative of Q w.r.t. proportion of natural areas (x_vals)
        dqp_dqm[r, c] = (sum(((qm[r, c] + dw) * x)^z * dvec, na.rm = TRUE) - sum(((qm[r, c]) * x)^z * dvec, na.rm =  TRUE)) / (dw * sum(dvec[which(!is.na(x))], na.rm=TRUE))
        dqm_dx[r, c] = -gamma * qm[r, c] * dhfi_dx[r, c]
        delta_qp[r, c] =  dqp_dqm[r, c] * dqm_dx[r, c]
      }
    }
  }

  # Sub-setting the matrix based in master_index
  it = as.vector(t(delta_qp))[master_index]
  #it = it/max(it, na.rm = T)

  # Replacing NA values with the mean of the total
  if (any(is.na(it))){warning("The calculated delta Q had NA")}
  it[is.na(it)] = mean(x = it, na.rm = T)
  message("\n")

  return(it)
}
