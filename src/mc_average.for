************************************************************
*  calculate the moisture content in each grid with time   *
************************************************************
      subroutine mc_average(rho_potash,mdot3,epss,dt,xxp1,sum)

	implicit doubleprecision (a-h,o-z)
	doubleprecision mdot3

       sum=xxp1-mdot3*dt/(1-epss)/rho_potash

	return
	end
