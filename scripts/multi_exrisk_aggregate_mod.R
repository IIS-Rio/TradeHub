#' @title Title...
#' @name multi_exrisk_aggregate
#'
#' @description Computes the total reduction in the extinction risk of a given
#' choice of priority areas
#'
#' @param x_vals .
#' @param prop_restore .
#' @param usphab_proc .
#' @param usphab_index .
#' @param hab_now_areas .
#' @param hab_pot_areas .
#' @param spp_main_range .
#' @param master_index .
#' @param z .
#' @param update_eps .
#' @param z_std_dev .
#' @param z_eps .
#' @param quad_unc .
#' @param ... ...
#'
#' @details para extincoes globais, areas de ocorrencia da especie fora do recorte da paisagem sendo otimizada precisam ser computadas e consideradas no calculo do risco de extincao, i_e_, o usuario precisa entrar com mapas de ocorrencia das especies recortados para a paisagem, mas tambem valores agregados de habitat atual e potencial totais, o que inclui possiveis areas de ocorrencia fora da paisagem_ Se tais areas de habitat n forem informadas, o script calculara exticoes locais (baseado apenas nos habitats atual e potencial dentro da paisagem)
#'
#' @return data.frame containing ...
#'
#' @author author
#'
#' @import progress
#'
#' @export

multi_exrisk_aggregate_mod = function(x_vals, prop_restore, usphab_proc, usphab_index,
                                  hab_now_areas, hab_pot_areas, spp_main_range,
                                  master_index, z = 0.25, update_eps = 1e-8,
                                  z_std_dev = 0.1, z_eps = 1.e-6, quad_unc = T,
                                  use_restor_weight = F,...) {

  # Aggregated value of biodiversity: sum of changes in extinction risk of species
  res = 0
  res_unc = 0
  # Starting BD layer
  bd = rep(0, nrow(prop_restore))
  # Looping over land-use/habitat combinations
  for (i in 1:nrow(as.data.frame(usphab_proc))) {

    # sum of habitat areas for the natural cover distribution given by usphab_proc[[i]]
    if(is.matrix(usphab_proc) & any(dim(usphab_proc)>2)){
      hab_values = prop_restore %*% usphab_proc[i,]
    } else {
      hab_values = prop_restore #* as.data.frame(usphab_proc)[i,]
    }

    # Using different weight for restored areas (Tom Brooks suggestion)
    if (use_restor_weight) {
      x_vals[x_vals > 0] = 0.87 * x_vals[x_vals > 0]
    }

    # change of natural area given by the solver run result
    delta_hab = unlist(x_vals, use.names = F) * hab_values[,1]

    # checking if there is any species that occurr within this land-use/habitat combination
    if (length(usphab_index[[i]])>0) {

      # Progress bar
      pb <- progress::progress_bar$new(
        format = "Extinction risk calculation [:bar] :percent in :elapsed",
        total = length(usphab_index[[i]]), clear = FALSE, width= 70)

      # looping over species that occurr within this land-use/habitat combination
      for (j in 1:length(usphab_index[[i]])) {
        pb$tick() # progress bar call
        j_spid = as.character(usphab_index[[i]][j]) # spp ID for j-iteration (species in usphab_index[[i]])
        # Species' habitat net change
        delta_hab_spid = sum(delta_hab[master_index %in% spp_main_range[[j_spid]]])
        # Current habitat
        hab_now_spid = hab_now_areas[[j_spid]]
        # Original habitat
        hab_pot_spid = hab_pot_areas[[j_spid]]

        if (is.null(hab_pot_spid)) {next}

        # Updating habitat of each species: nullifying updated habitat if the
        # sum of current and delta habitat is below a threshold (there are rounding
        # errors when updating habitat yielding to negative updated habitats in some cases)
        hab_updated_spid = ifelse((abs(hab_now_spid + delta_hab_spid) < update_eps), 0, hab_now_spid + delta_hab_spid)

        # Computing change in extinction risk of each j_spid
        delta_exrisk = unlist(multi_extinction_risk(A = hab_now_spid, Amax = hab_pot_spid, z = z), use.names = F) -
          unlist(multi_extinction_risk(A = hab_updated_spid, Amax = hab_pot_spid, z = z), use.names = F)

        # Removing defect species
        if (is.na(delta_exrisk)) { delta_exrisk = 0 }

        # essa parte aqui eh a espacializacao copiada da (multi_calc_bd)(chico)
        # ainda nao funcionou, mas de alguma forma tenho q colocar o delta ai no bd!
        # Master_index indices where species with j_spid occurs
        j_index = (master_index %in% spp_main_range[[j_spid]])

        # bd subset j_index are updated by spid slope times

        res = res + delta_exrisk
        bd[j_index] = res

        # Computing change in extinction risk of each j_spid for a z_eps variation ----
        # delta_exrisk_eps = unlist(multi_extinction_risk(A = hab_now_spid, Amax = hab_pot_spid, z = z + z_eps), use.names = F) - unlist(multi_extinction_risk(A = hab_updated_spid, Amax = hab_pot_spid, z = z + z_eps), use.names = F)

        # Derivative of the extinction risk w.r.t. the z parameter
        #d_exrisk_dz = (delta_exrisk_eps - delta_exrisk) / z_eps

        #if (quad_unc) {
        #  delta_exrisk_unc = (d_exrisk_dz)^2 * (z_std_dev)^2
        #} else {
        #  delta_exrisk_unc = abs((d_exrisk_dz) * (z_std_dev))
        #}

        #res_unc = res_unc + delta_exrisk_unc

        #if (quad_unc) {res_unc = sqrt(res_unc)}

      }
    }
  }
  return(bd)
}


