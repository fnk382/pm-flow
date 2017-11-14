**************************************************
*   calculation of the enthalpy change in had    *
**************************************************

	  subroutine enthalpy(xp,hfg,had)
	  implicit doubleprecision (a-h,o-z)
	  doubleprecision xp,hfg,had
	
	  if(xp.le.1d-02) then
	
	  had=(-0.48*(xp*100)**3+1.77*(xp*100)**2-2.11*(xp*100)+
     *            1.71)*hfg

	  else

	  had=0.89*hfg

	  end if

	  return
	  end