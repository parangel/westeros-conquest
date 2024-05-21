dir.create(path, showWarnings = FALSE)
png(paste0(path, "%d.png"), width = 620, height = 1240)
par(mai = rep(0, 4), bg = "#252525")
plot_map(map_type, army_col, "#252525")

new_turn <- TRUE
while (new_turn) {
	turn <- choose_army()
	army <- turn$army
	turn_weights <- turn$weights

	troops <- troops + reinforce_troops()
	battlefield <- choose_battlefield()

	if (!is.null(battlefield)) {
		if (control[battlefield[1]] == control[battlefield[2]]) {
			browser()
		}

		battle_result <- battle()
		troops[battlefield] <- battle_result$troops

		if (battle_result$attacker_won) {
			defeated <- control[battlefield[2]]
			control[battlefield[2]] <- army

			turn_weights[defeated] <- turn_weights[defeated] + runif(1)

			# plot_territory(battlefield[2], army_col[army], "#252525")


			if (!any(control == defeated)) {
				# cat(sprintf("%s was defeated.\n", armies_name[defeated]))
				armies <- setdiff(armies, defeated)
				Sys.sleep(1)

				if (length(armies) == 1) {
					# cat(sprintf("%s won.\n", armies_name[army]))
					new_turn <- FALSE
				}
			}
		}
	}

	plot_map(map_type, army_col, "#252525")

	# Sys.sleep(sleep_time)
}

dev.off()
