! This file holds the user analysis routine and related functions
! and subroutines:
module analysis_supp
implicit none
!
! integer , parameter :: nbins_jetrates = 100
  integer , parameter :: nbins_jetrates = 130
  real(kind(1d0)) , dimension(nbins_jetrates+1) :: bins_jetrates = &
!Original histogram binning by Weinzierl
! (/1.05925d-5,1.18850d-5,1.33352d-5,1.49624d-5,1.67880d-5, &
!   1.88365d-5,2.11349d-5,2.37137d-5,2.66073d-5,2.98538d-5, &
!   3.34965d-5,3.75837d-5,4.21696d-5,4.73150d-5,5.30884d-5, &
!   5.95662d-5,6.68344d-5,7.49894d-5,8.41395d-5,9.44061d-5, &
!   1.05925d-4,1.18850d-4,1.33352d-4,1.49624d-4,1.67880d-4, &
!   1.88365d-4,2.11349d-4,2.37137d-4,2.66073d-4,2.98538d-4, &
!   3.34965d-4,3.75837d-4,4.21697d-4,4.73151d-4,5.30884d-4, &
!   5.95662d-4,6.68344d-4,7.49894d-4,8.41395d-4,9.44061d-4, &
!   1.05925d-3,1.18850d-3,1.33352d-3,1.49624d-3,1.67880d-3, &
!   1.88365d-3,2.11349d-3,2.37136d-3,2.66073d-3,2.98538d-3, &
!   3.34965d-3,3.75837d-3,4.21696d-3,4.73151d-3,5.30884d-3, &
!   5.95662d-3,6.68344d-3,7.49894d-3,8.41395d-3,9.44061d-3, &
!   1.05925d-2,1.18850d-2,1.33352d-2,1.49624d-2,1.67880d-2, &
!   1.88365d-2,2.11349d-2,2.37137d-2,2.66073d-2,2.98538d-2, &
!   3.34965d-2,3.75837d-2,4.21697d-2,4.73151d-2,5.30884d-2, &
!   5.95662d-2,6.68344d-2,7.49894d-2,8.41395d-2,9.44061d-2, &
!   1.05925d-1,1.18850d-1,1.33352d-1,1.49624d-1,1.67880d-1, &
!   1.88365d-1,2.11349d-1,2.37137d-1,2.66073d-1,2.98538d-1, &
!   3.34965d-1,3.75837d-1,4.21697d-1,4.73151d-1,5.30884d-1, &
!   5.95662d-1,6.68344d-1,7.49894d-1,8.41395d-1,9.44061d-1, &
!   1d0/)
!Extended histogram
 (/3.34965d-7,3.75837d-7,4.21697d-7,4.73151d-7,5.30884d-7, &
   5.95662d-7,6.68344d-7,7.49894d-7,8.41395d-7,9.44061d-7, &
   1.05925d-6,1.1885d-6,1.33352d-6,1.49624d-6,1.6788d-6, &
   1.88365d-6,2.11349d-6,2.37137d-6,2.66073d-6,2.98538d-6, &
   3.34965d-6,3.75837d-6,4.21697d-6,4.73151d-6,5.30884d-6, &
   5.95662d-6,6.68344d-6,7.49894d-6,8.41395d-6,9.44061d-6, &
   0.0000105925d0,0.000011885d0,0.0000133352d0,0.0000149624d0,0.000016788d0, &
   0.0000188365d0,0.0000211349d0,0.0000237137d0,0.0000266073d0,0.0000298538d0, &
   0.0000334965d0,0.0000375837d0,0.0000421697d0,0.0000473151d0,0.0000530884d0, &
   0.0000595662d0,0.0000668344d0,0.0000749894d0,0.0000841395d0,0.0000944061d0, &
   0.000105925d0,0.00011885d0,0.000133352d0,0.000149624d0,0.00016788d0, &
   0.000188365d0,0.000211349d0,0.000237137d0,0.000266073d0,0.000298538d0, &
   0.000334965d0,0.000375837d0,0.000421697d0,0.000473151d0,0.000530884d0, &
   0.000595662d0,0.000668344d0,0.000749894d0,0.000841395d0,0.000944061d0, &
   0.00105925d0,0.0011885d0,0.00133352d0,0.00149624d0,0.0016788d0, &
   0.00188365d0,0.00211349d0,0.00237137d0,0.00266073d0,0.00298538d0, &
   0.00334965d0,0.00375837d0,0.00421697d0,0.00473151d0,0.00530884d0, &
   0.00595662d0,0.00668344d0,0.00749894d0,0.00841395d0,0.00944061d0, &
   0.0105925d0,0.011885d0,0.0133352d0,0.0149624d0,0.016788d0, &
   0.0188365d0,0.0211349d0,0.0237137d0,0.0266073d0,0.0298538d0, &
   0.0334965d0,0.0375837d0,0.0421697d0,0.0473151d0,0.0530884d0, &
   0.0595662d0,0.0668344d0,0.0749894d0,0.0841395d0,0.0944061d0, &
   0.105925d0,0.11885d0,0.133352d0,0.149624d0,0.16788d0, &
   0.188365d0,0.211349d0,0.237137d0,0.266073d0,0.298538d0, &
   0.334965d0,0.375837d0,0.421697d0,0.473151d0,0.530884d0, &
   0.595662d0,0.668344d0,0.749894d0,0.841395d0,0.944061d0, &
   1d0/)
!
end module analysis_supp
!
! This analysis corresponds to the one published in arXiv:0904.1077:
!
subroutine init_analysis
use histo
use analysis_supp
use scales
implicit none
!
!
  integer :: ialgo
!
!
! We have to initialize the histograms:
  call init_hist(nscales)
!
! Exclusive Duhram:
  call bookup_hist("/jetrates/IncDurham3jet-Ecut1","MCCSM", &
                   nbins_jetrates,bins_jetrates)
  call bookup_hist("/jetrates/IncDurham3jet-Ecut2","MCCSM", &
                   nbins_jetrates,bins_jetrates)
  call bookup_hist("/jetrates/IncDurham3jet-Ecut3","MCCSM", &
                   nbins_jetrates,bins_jetrates)
! call bookup_hist("/jetrates/IncDurhamExc4jet","MCCSM", &
!                  nbins_jetrates,bins_jetrates)
! call bookup_hist("/jetrates/IncDurhamExc5jet","MCCSM", &
!                  nbins_jetrates,bins_jetrates)
!
end subroutine init_analysis
!
subroutine analysis(p,wgt)
use histo
use particles
use observables
use analysis_supp
use math
implicit none
!
  type(particle) , dimension(:) , intent(in) :: p
  real(kind(1d0)) , dimension(:) , intent(in) :: wgt
!
  integer , parameter :: maxjet = 5
  integer, parameter :: nEmin = 3
!
  integer :: ipart,itrack,ntrack,i,j,nsoft
  real(kind(1d0)) :: Q2, R
  real(kind(1d0)), dimension(nEmin) :: Emin
  integer, dimension(nEmin) :: njets
  real(kind(1d0)) , dimension(4,maxjet) :: ptrack
  real(kind(1d0)), dimension(maxjet) :: Yn
  real(kind(1d0)), dimension(maxjet,maxjet) :: En
!
!
  Q2 = 2*p(1)%p*p(2)%p
!
!
  ptrack = 0
  ntrack = 0
  do ipart=3,size(p)
    ntrack = ntrack + 1
    ptrack(1,ntrack) = p(ipart)%p%px
    ptrack(2,ntrack) = p(ipart)%p%py
    ptrack(3,ntrack) = p(ipart)%p%pz
    ptrack(4,ntrack) = p(ipart)%p%E
  end do
!
!The following iteration might be slow, but probably faster than using FastJet
!
!Use multiple energy cuts
  Emin(1) = 7.7d-2*sqrt(Q2)
  Emin(2) = 7.7d-3*sqrt(Q2)
  Emin(3) = 7.7d-4*sqrt(Q2)
!New algorithm
  call calc_inc_durham_yij(ntrack,ptrack,Yn,En)
!
!Loop over different energy cuts
  do i=1,nEmin
!Contribution from 3 jet final state
   nsoft=0
   if (ntrack.ge.3) then
! energy cut for 3 jets
    call applyEcut(ntrack,En(1,:),Emin(i),nsoft)
! 3 jets with no soft particles
    if (nsoft.eq.0) then
     call fill_hist(i,Yn(1),wgt)
    end if
   end if
!Contribution from 4 jet final state
   if (ntrack.ge.4) then
! 4->3 jets with no soft particles
! nsoft still has its value related to 3 jet energies
    if (nsoft.eq.0) then
     call fill_hist(i,Yn(2),-wgt)
    end if
! energy cut for 4 jets
    call applyEcut(ntrack,En(2,:),Emin(i),nsoft)
! 4 jets with 1 soft particle
    if (nsoft.eq.1) then
     call fill_hist(i,Yn(2),wgt)
    end if
   end if
!Contribution from 5 jet final state
   if (ntrack.ge.5) then
! 5->4 jets with 1 soft particle
! nsoft still has its value related to 4 jet energies
    if(nsoft.eq.1) then
     call fill_hist(i,Yn(3),-wgt)
    end if
! energy cut for 5 jets
    call applyEcut(ntrack,En(3,:),Emin(i),nsoft)
! 5 jets with 2 soft particle
    if (nsoft.eq.2) then
     call fill_hist(i,Yn(3),wgt)
    end if
   end if
!
  end do
!
end subroutine analysis
!
!This is small subroutine which returns the number of jets which are below the energy cut
subroutine applyEcut(ntracks,Ei,Ecut,n)
implicit none
!
 integer, intent(in) :: ntracks
 real(kind(1d0)), dimension(ntracks), intent(in) :: Ei
 real(kind(1d0)), intent(in) :: Ecut
 integer, intent(out) :: n
!
 integer :: i
!
 n=0
!
 do i=1,ntracks
  if((Ei(i).gt.0).AND.(Ei(i).lt.Ecut)) then
   n = n + 1
  end if
 end do
!
end subroutine applyEcut
!
!These subroutines implements the the generalised inclusive
!kt algorithms with p=1,0,-1
!
subroutine calc_inc_durham_yij(ntracks,ptracks,ymin,Yn,En)
implicit none
!
! I/O
  integer, intent(in) :: ntracks
  real(kind(1d0)), dimension(4,ntracks), intent(in) :: ptracks
  real(kind(1d0)), intent(in) :: ymin
  real(kind(1d0)), dimension(ntracks), intent(out) :: Yn
  real(kind(1d0)), dimension(5,5), intent(out) :: En !limitation to 5 jets
!internal variables
  integer :: i, j, itrk, jtrk,njet
  real(kind(1d0)) :: dij, diB, dij_min, diB_min, yij, yij_max, ycut
  real(kind(1d0)) :: Ei, Ej, piabs, pjabs, costhetaij
  real(kind(1d0)), dimension(4,ntracks) :: pjet 
  integer, dimension(ntracks) :: jetvec
  real(kind(1d0)), dimension(5) :: Ei_vec
  real(kind(1d0)), dimension(4) :: tmp_p
!
  Yn = 0
  En = 0
!
!Set yij_max to 1
  yij_max=1
!
  do while(.true.)
!Copy momenta
   pjet = ptracks
!Everyone is a pseudojet: 0
   jetvec = 0
   Ei_vec = 0
   njet = 0
!ycut is yij_max taken from the last iteration
   ycut = yij_max
!yij_max reset to its minimum
   yij_max = 0
!We terminate if:
! - ycut<ymin
   if (ycut.lt.ymin) exit
! - or njets == ntracks
   if (njet.eq.ntracks) exit
!
!Loop for clustering
   do while(.true.)
!Init some huge dmin values
     dij_min = 1d20
     diB_min = 1d20
!
!First compute the measures and find the minimum diB and dij
       if (jetvec(i).ne.0) cycle
       Ei = pjet(4,i)
       diB = Ei**2
       piabs = sqrt(sum(pjet(1:3,i)**2))
       do j=i+1,ntracks
         if (jetvec(j).ne.0) cycle
         Ej = pjet(4,j)
         pjabs = sqrt(sum(pjet(1:3,j)**2))
         if ((piabs.eq.0).or.(pjabs.eq.0)) then
           costhetaij = 1
         else
           costhetaij = (pjet(1,i)*pjet(1,j) + pjet(2,i)*pjet(2,j) &
                      +  pjet(3,i)*pjet(3,j))/(piabs*pjabs)
         end if
!Note: ycut = 1 - cos(R)
         dij = min(Ei**2,Ej**2)*(1 - costhetaij)/ycut
         yij = min(Ei**2,Ej**2)*(1 - costhetaij)/diB
!Find yij_max
         if((yij.gt.yij_max).AND.(yij.lt.ycut)) then
           yij_max = yij
         end if
         if (dij.lt.dij_min) then
           itrk = i
           jtrk = j
           dij_min = dij
         end if   
       end do
       if (diB.lt.diB_min) then
         iBtrk = i
         diB_min = diB
       end if
     end do
!
!Decide what to do:
!Combine particles
     if(dij_min.lt.diB_min) then
       tmp_p = pjet(:,itrk) + pjet(:,jtrk)
       pjet(:,itrk) = tmp_p
!  particle jtrk doesnt play in the game anymore
       jetvec(jtrk) = -1
       pjet(:,jtrk) = 0
     else
!Jet i become an inclusive jet!
       jetvec(iBtrk) = 1
     end if
!Count resolved jets
     njet = 0
     do i=1,ntracks
      if (jetvec(i).eq.0) then
       njet = njet + 1
      end if
     end do
! If the number of jets is 1, quit the loop
     if (njet.eq.1) exit 
   end do

  end do
!Clusterization done!
!Energy cuts are done outside of this routine
!
end subroutine calc_inc_durham_yij
!
!
subroutine cluster_inc_durham(ntracks,ptracks,nEmin,Emin,ycut,njets)
implicit none
!
! I/O
  integer, intent(in) :: ntracks
!NOTE: size of ptracks is limited to 5 which corresponds to maxjet in the analysis routine!
  real(kind(1d0)), dimension(4,5), intent(in) :: ptracks
  integer, intent(in) :: nEmin
  real(kind(1d0)), dimension(nEmin), intent(in) :: Emin
  real(kind(1d0)), intent(in) :: ycut
  integer, dimension(nEmin), intent(out) :: njets
!internal variables
  integer :: i, j, itrk, jtrk, iBtrk,npseudo, counter
  real(kind(1d0)) :: dij, diB, dij_min, diB_min
  real(kind(1d0)) :: Ei, Ej, piabs, pjabs, costhetaij
  real(kind(1d0)), dimension(4,ntracks) :: pjet 
  integer, dimension(ntracks) :: jetvec
  real(kind(1d0)), dimension(4) :: tmp_p

!Copy momenta
 do i=1,ntracks
   pjet(:,i) = ptracks(:,i)
 end do
!call exit(0)
!
 jetvec = 0
 npseudo = ntracks
!
!Loop until everyone becomes a jet
  do while(.true.)
!Init some huge dmin values
  dij_min = 1d20
  diB_min = 1d20
! counter = 0
!Loop counter to catch infinite loop
! counter = counter + 1
! if(counter.gt.200) then
!   print *, "I'm stucked in the freakin loop!!!"
!   call exit(0)
! end if
!
!First compute the measures and find the minimum
    do i=1,ntracks
      if (jetvec(i).ne.0) cycle
      Ei = pjet(4,i)
      diB = Ei**2
      piabs = sqrt(sum(pjet(1:3,i)**2))
      do j=i+1,ntracks
        if (jetvec(j).ne.0) cycle
        Ej = pjet(4,j)
        pjabs = sqrt(sum(pjet(1:3,j)**2))
        if ((piabs.eq.0).or.(pjabs.eq.0)) then
          costhetaij = 1
        else
          costhetaij = (pjet(1,i)*pjet(1,j) + pjet(2,i)*pjet(2,j) &
                     +  pjet(3,i)*pjet(3,j))/(piabs*pjabs)
        end if
!Note: ycut = 1 - cos(R)
        dij = min(Ei**2,Ej**2)*(1 - costhetaij)/ycut
        if (dij.lt.dij_min) then
          itrk = i
          jtrk = j
          dij_min = dij
        end if   
      end do
      if (diB.lt.diB_min) then
        iBtrk = i
        diB_min = diB
      end if
    end do
!
!
!Decide what to do:
!Combine particles
    if(dij_min.lt.diB_min) then
      tmp_p = pjet(:,itrk) + pjet(:,jtrk)
      pjet(:,itrk) = tmp_p
!  particle jtrk doesnt play in the game anymore
      jetvec(jtrk) = -1
      pjet(:,jtrk) = 0
    else
!Jet i become an inclusive jet!
      jetvec(iBtrk) = 1
! decrease the number of pseudo jets
      npseudo = npseudo - 1
    end if
    if (npseudo.eq.0) exit
  end do
!Clusterization done!
!
!Apply energy cut to get the actual number of jets
!  At this point every jetvec value is -1 or 1
!  jets with jetvec -1 are always below the energy cut
!  since they are nullvectors
! We loop over multiple energy cuts
  do j=1,nEmin
   njets(j) = 0
   do i=1,ntracks
     if(pjet(4,i).gt.Emin(j)) then
       njets(j) = njets(j) + 1
     end if
   end do
  end do
!
end subroutine cluster_inc_durham
!
!
subroutine cluster_inc_aachen(ntracks,ptracks,nEmin,Emin,ycut,njets)
implicit none
!
! I/O
  integer, intent(in) :: ntracks
!NOTE: size of ptracks is limited to 5 which corresponds to maxjet in the analysis routine!
  real(kind(1d0)), dimension(4,5), intent(in) :: ptracks
  integer, intent(in) :: nEmin
  real(kind(1d0)), dimension(nEmin), intent(in) :: Emin
  real(kind(1d0)), intent(in) :: ycut
  integer, dimension(nEmin), intent(out) :: njets
!internal variables
  integer :: i, j, itrk, jtrk, counter
  real(kind(1d0)) :: dij, dij_min
  real(kind(1d0)) :: piabs, pjabs, costhetaij
  real(kind(1d0)), dimension(4,ntracks) :: pjet 
  integer, dimension(ntracks) :: jetvec
  real(kind(1d0)), dimension(4) :: tmp_p

!Copy momenta
 do i=1,ntracks
   pjet(:,i) = ptracks(:,i)
 end do
!
 jetvec = 0
!
!Loop until everyone becomes a jet
  do while(.true.)
!Init some huge dmin values
  dij_min = 1d20
!diB_min is always 1
! diB_min = 1d0
! counter = 0
!Loop counter to catch infinite loop
! counter = counter + 1
! if(counter.gt.200) then
!   print *, "I'm stucked in the freakin loop!!!"
!   call exit(0)
! end if
!
!First compute the measures and find the minimum
    do i=1,ntracks
      if (jetvec(i).ne.0) cycle
      piabs = sqrt(sum(pjet(1:3,i)**2))
      do j=i+1,ntracks
        if (jetvec(j).ne.0) cycle
        pjabs = sqrt(sum(pjet(1:3,j)**2))
        if ((piabs.eq.0).or.(pjabs.eq.0)) then
          costhetaij = 1
        else
          costhetaij = (pjet(1,i)*pjet(1,j) + pjet(2,i)*pjet(2,j) &
                     +  pjet(3,i)*pjet(3,j))/(piabs*pjabs)
        end if
!Note: ycut = 1 - cos(R)
        dij = (1 - costhetaij)/ycut
        if (dij.lt.dij_min) then
          itrk = i
          jtrk = j
          dij_min = dij
        end if   
      end do
    end do
!
!
!Decide what to do:
!Combine particles
    if(dij_min.lt.1d0) then
      tmp_p = pjet(:,itrk) + pjet(:,jtrk)
      pjet(:,itrk) = tmp_p
!  particle jtrk doesnt play in the game anymore
      jetvec(jtrk) = -1
      pjet(:,jtrk) = 0
    else
!We exit the loop
      exit 
    end if
! decrease the number of pseudo jets
  end do
!Clusterization done!
!
!Apply energy cut to get the actual number of jets
!  At this point every jetvec value is -1 or 1
!  jets with jetvec -1 are always below the energy cut
!  since they are nullvectors
! We loop over multiple energy cuts
  do j=1,nEmin
   njets(j) = 0
   do i=1,ntracks
     if(pjet(4,i).gt.Emin(j)) then
       njets(j) = njets(j) + 1
     end if
   end do
  end do
!
end subroutine cluster_inc_aachen
!
!
subroutine cluster_inc_antikt(ntracks,ptracks,nEmin,Emin,ycut,njets)
implicit none
!
! I/O
  integer, intent(in) :: ntracks
!NOTE: size of ptracks is limited to 5 which corresponds to maxjet in the analysis routine!
  real(kind(1d0)), dimension(4,5), intent(in) :: ptracks
  integer, intent(in) :: nEmin
  real(kind(1d0)), dimension(nEmin), intent(in) :: Emin
  real(kind(1d0)), intent(in) :: ycut
  integer, dimension(nEmin), intent(out) :: njets
!internal variables
  integer :: i, j, itrk, jtrk, iBtrk,npseudo, counter
  real(kind(1d0)) :: dij, diB, dij_min, diB_min
  real(kind(1d0)) :: Ei, Ej, piabs, pjabs, costhetaij
  real(kind(1d0)), dimension(4,ntracks) :: pjet 
  integer, dimension(ntracks) :: jetvec
  real(kind(1d0)), dimension(4) :: tmp_p

!Copy momenta
 do i=1,ntracks
   pjet(:,i) = ptracks(:,i)
 end do
!
 jetvec = 0
 npseudo = ntracks
!
!Loop until everyone becomes a jet
  do while(.true.)
!Init some huge dmin values
  dij_min = 1d20
  diB_min = 1d20
! counter = 0
!Loop counter to catch infinite loop
! counter = counter + 1
! if(counter.gt.200) then
!   print *, "I'm stucked in the freakin loop!!!"
!   call exit(0)
! end if
!
!First compute the measures and find the minimum
    do i=1,ntracks
      if (jetvec(i).ne.0) cycle
      Ei = pjet(4,i)
      diB = 1/(Ei**2)
      piabs = sqrt(sum(pjet(1:3,i)**2))
      do j=i+1,ntracks
        if (jetvec(j).ne.0) cycle
        Ej = pjet(4,j)
        pjabs = sqrt(sum(pjet(1:3,j)**2))
        if ((piabs.eq.0).or.(pjabs.eq.0)) then
          costhetaij = 1
        else
          costhetaij = (pjet(1,i)*pjet(1,j) + pjet(2,i)*pjet(2,j) &
                     +  pjet(3,i)*pjet(3,j))/(piabs*pjabs)
        end if
!Note: ycut = 1 - cos(R)
        dij = min(1/(Ei**2),1/(Ej**2))*(1 - costhetaij)/ycut
        if (dij.lt.dij_min) then
          itrk = i
          jtrk = j
          dij_min = dij
        end if   
      end do
      if (diB.lt.diB_min) then
        iBtrk = i
        diB_min = diB
      end if
    end do
!
!
!Decide what to do:
!Combine particles
    if(dij_min.lt.diB_min) then
      tmp_p = pjet(:,itrk) + pjet(:,jtrk)
      pjet(:,itrk) = tmp_p
!  particle jtrk doesnt play in the game anymore
      jetvec(jtrk) = -1
      pjet(:,jtrk) = 0
    else
!Jet i become an inclusive jet!
      jetvec(iBtrk) = 1
! decrease the number of pseudo jets
      npseudo = npseudo - 1
    end if
    if (npseudo.eq.0) exit
  end do
!Clusterization done!
!
!Apply energy cut to get the actual number of jets
!  At this point every jetvec value is -1 or 1
!  jets with jetvec -1 are always below the energy cut
!  since they are nullvectors
! We loop over multiple energy cuts
  do j=1,nEmin
   njets(j) = 0
   do i=1,ntracks
     if(pjet(4,i).gt.Emin(j)) then
       njets(j) = njets(j) + 1
     end if
   end do
  end do
!
end subroutine cluster_inc_antikt
!
! This is a general routine calculating all the n-to-n-1
! jet transition variables:
subroutine CalcYn(algo,Q2,ntrack,ptrack,Yn)
implicit none
!
  integer , intent(in) :: algo
  real(kind(1d0)) , intent(in) :: Q2
  integer , intent(in) :: ntrack
  real(kind(1d0)) , dimension(4,ntrack) , intent(in) :: ptrack
  real(kind(1d0)) , dimension(ntrack) , intent(out) :: Yn
!
  integer :: itrack,i,j,itrk,jtrk,mjet
  integer , dimension(ntrack) :: jetvec
  real(kind(1d0)) :: vijmin,vij,yij,piabs,pjabs,pijabs,costhetaij,Ei,Ej
  real(kind(1d0)) , dimension(4) :: p_tmp
  real(kind(1d0)) , dimension(4,ntrack) :: pjet
!
!
  Yn = 0
!
! Copy the tracks into the jets array:
  pjet = ptrack
! Defining a jetvec to keep track of which pseudojets are combined away
! and which are still present:
  jetvec = 1
! When jetvec equals -1 it means the particle is recombined with an
! other one, if it is 0 it is a resolved jet (used in the 
! Cambridge algo., though not used here...)
!
! In the zeroth iteration we have as many jets as tracks,
! hence if we have n tracks the distance to resolve n+1 jets
! is exactly zero:
  yij = 0
!
  do while (.true.)
! Counting the jets:
    mjet = 0
    do i=1,ntrack
! Skipping recombined pseudojets:
      if (jetvec(i).eq.-1) cycle
      mjet = mjet + 1
    end do
! If the number of remaining jets is less than 2 we quit:
    if (mjet.lt.2) return
! The y value where the transition happens from a mjet + 1
! jet configuration to a mjet configuration is stored in yij:
! Note, that the 3-to-2 jet transition is stored in the first
! position:
    Yn(mjet-1) = yij
! vijmin is the minimal distance found in the current 
! iteration which corresponds to the yij except for the 
! Cambridge algorithm:
    vijmin = 1d99
    do i=1,ntrack-1
      if (jetvec(i).eq.-1) cycle
      Ei = pjet(4,i)
      piabs = sqrt(sum(pjet(1:3,i)**2))
      do j=i+1,ntrack
        if (jetvec(j).eq.-1) cycle
        Ej = pjet(4,j)
        pjabs = sqrt(sum(pjet(1:3,j)**2))
        if ((piabs.eq.0).or.(pjabs.eq.0)) then
          costhetaij = 1
        else
          costhetaij = (pjet(1,i)*pjet(1,j) + pjet(2,i)*pjet(2,j) &
                     +  pjet(3,i)*pjet(3,j))/(piabs*pjabs)
        end if
! Calculation of vij:
! Durham-algo, vij = yij:
        if (algo.eq.1) then
          vij = 2*min(Ei**2,Ej**2)*(1 - costhetaij)/Q2
! Geneva-algo, vij = yij:
        elseif (algo.eq.2) then
          vij = 8d0/9d0*Ei*Ej*(1 - costhetaij)/(Ei + Ej)**2
! Jade-algo with E0 or with E, vij = yij:
        elseif ((algo.eq.3).or.(algo.eq.4)) then
          vij = 2*Ei*Ej*(1 - costhetaij)/Q2
! Cambridge-algo, vij != yij:
        elseif (algo.eq.5) then
          vij = 2*(1 - costhetaij)
          print *,"Error!!!! This routine cannot be used with this algo"
          stop "CalcYn"
        else
          print *,"Error is CalcYn, algorithm is not implemented yet"
          stop "CalcYn..."
        end if
        if (vij.lt.vijmin) then
          itrk = i
          jtrk = j
          vijmin = vij
!          print *,"itrk,jtrk: ",itrk,jtrk
!          print *,"vijmin changed: ",vijmin
        end if
      end do
    end do
! If the algorithm was the Cambridge one vij is defined
! differently from yij hence yij has to be calculated:
    if (algo.eq.5) then
      Ei = pjet(4,itrk)
      Ej = pjet(4,jtrk)
      piabs = sqrt(sum(pjet(1:3,itrk)**2))
      pjabs = sqrt(sum(pjet(1:3,jtrk)**2))
      if ((piabs.eq.0).or.(pjabs.eq.0)) then
        costhetaij = 1
      else
        costhetaij = (pjet(1,itrk)*pjet(1,jtrk) + pjet(2,itrk)*pjet(2,jtrk) &
                   + pjet(3,itrk)*pjet(3,jtrk))/(piabs*pjabs)
      end if
      yij = 2*min(Ei**2,Ej**2)*(1 - costhetaij)/Q2
! Otherwise yij is just vijmin:
    else
      yij = vijmin
    end if
! We calculate transition functions, hence the pair with the
! smallest separation is always recombined:
! The recombination scheme varies from algo to algo:
! For Durham, Geneva,Jade-E and Cambridge we use the E-scheme:
    if (algo.ne.3) then
      p_tmp = pjet(:,itrk) + pjet(:,jtrk)
! For Jade-E0 which is algo = 3 we use the E0-scheme:
    else
      p_tmp = pjet(:,itrk) + pjet(:,jtrk)
! The spatial part has to be rescaled:
      pijabs = sqrt(sum(p_tmp(1:3)**2))
      p_tmp(1:3) = p_tmp(4)/pijabs*p_tmp(1:3)
    end if
    pjet(:,itrk) = p_tmp
! Nullifying the jet momentum:
    pjet(:,jtrk) = 0
! And putting the actual jetvec item to -1:
    jetvec(jtrk) = -1
  end do
!
end subroutine CalcYn
!
