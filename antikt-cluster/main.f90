program main

implicit none
 
include 'seed.f'

!---------HISTOGRAMS---------
!
! 120 bins
  integer, parameter :: nbins = 120
  real(kind(1d0)) , dimension(nbins) :: ycut_bins = &
 (/ 1.05925d-6,1.1885d-6,1.33352d-6,1.49624d-6,1.6788d-6, &
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
   0.595662d0,0.668344d0,0.749894d0,0.841395d0,0.944061d0/)
!
! 90 bins
! integer, parameter :: nbins = 90
! real(kind(1d0)) , dimension(nbins) :: ycut_bins = &
!(/0.0000334965d0,0.0000375837d0,0.0000421697d0,0.0000473151d0,0.0000530884d0, &
!  0.0000595662d0,0.0000668344d0,0.0000749894d0,0.0000841395d0,0.0000944061d0, &
!  0.000105925d0,0.00011885d0,0.000133352d0,0.000149624d0,0.00016788d0, &
!  0.000188365d0,0.000211349d0,0.000237137d0,0.000266073d0,0.000298538d0, &
!  0.000334965d0,0.000375837d0,0.000421697d0,0.000473151d0,0.000530884d0, &
!  0.000595662d0,0.000668344d0,0.000749894d0,0.000841395d0,0.000944061d0, &
!  0.00105925d0,0.0011885d0,0.00133352d0,0.00149624d0,0.0016788d0, &
!  0.00188365d0,0.00211349d0,0.00237137d0,0.00266073d0,0.00298538d0, &
!  0.00334965d0,0.00375837d0,0.00421697d0,0.00473151d0,0.00530884d0, &
!  0.00595662d0,0.00668344d0,0.00749894d0,0.00841395d0,0.00944061d0, &
!  0.0105925d0,0.011885d0,0.0133352d0,0.0149624d0,0.016788d0, &
!  0.0188365d0,0.0211349d0,0.0237137d0,0.0266073d0,0.0298538d0, &
!  0.0334965d0,0.0375837d0,0.0421697d0,0.0473151d0,0.0530884d0, &
!  0.0595662d0,0.0668344d0,0.0749894d0,0.0841395d0,0.0944061d0, &
!  0.105925d0,0.11885d0,0.133352d0,0.149624d0,0.16788d0, &
!  0.188365d0,0.211349d0,0.237137d0,0.266073d0,0.298538d0, &
!  0.334965d0,0.375837d0,0.421697d0,0.473151d0,0.530884d0, &
!  0.595662d0,0.668344d0,0.749894d0,0.841395d0,0.944061d0/)
!
! 60 bins
! integer, parameter :: nbins = 60
! real(kind(1d0)) , dimension(nbins) :: ycut_bins = &
!(/0.00105925d0,0.0011885d0,0.00133352d0,0.00149624d0,0.0016788d0, &
!  0.00188365d0,0.00211349d0,0.00237137d0,0.00266073d0,0.00298538d0, &
!  0.00334965d0,0.00375837d0,0.00421697d0,0.00473151d0,0.00530884d0, &
!  0.00595662d0,0.00668344d0,0.00749894d0,0.00841395d0,0.00944061d0, &
!  0.0105925d0,0.011885d0,0.0133352d0,0.0149624d0,0.016788d0, &
!  0.0188365d0,0.0211349d0,0.0237137d0,0.0266073d0,0.0298538d0, &
!  0.0334965d0,0.0375837d0,0.0421697d0,0.0473151d0,0.0530884d0, &
!  0.0595662d0,0.0668344d0,0.0749894d0,0.0841395d0,0.0944061d0, &
!  0.105925d0,0.11885d0,0.133352d0,0.149624d0,0.16788d0, &
!  0.188365d0,0.211349d0,0.237137d0,0.266073d0,0.298538d0, &
!  0.334965d0,0.375837d0,0.421697d0,0.473151d0,0.530884d0, &
!  0.595662d0,0.668344d0,0.749894d0,0.841395d0,0.944061d0/)
!
! 30 bins
! integer, parameter :: nbins = 30
! real(kind(1d0)) , dimension(nbins) :: ycut_bins = &
!(/0.0334965d0,0.0375837d0,0.0421697d0,0.0473151d0,0.0530884d0, &
!  0.0595662d0,0.0668344d0,0.0749894d0,0.0841395d0,0.0944061d0, &
!  0.105925d0,0.11885d0,0.133352d0,0.149624d0,0.16788d0, &
!  0.188365d0,0.211349d0,0.237137d0,0.266073d0,0.298538d0, &
!  0.334965d0,0.375837d0,0.421697d0,0.473151d0,0.530884d0, &
!  0.595662d0,0.668344d0,0.749894d0,0.841395d0,0.944061d0/)

!Parameters
  real(kind(1d0)), parameter :: Q=1d2
  integer, parameter :: nEcut = 1
  real(kind(1d0)),dimension(nEcut) :: Ecut=(/8d0/)
!
!Number of particles
  integer, parameter :: npart=20
!Number of phase space points
  integer, parameter :: nps=1000
  real(kind(1d0)), parameter :: p=-1
  integer, parameter :: ee=1
!Variables
  real(kind(1d0)), dimension(4,npart) :: momenta
  real(kind(1d0)), dimension(npart) :: masses
  real(kind(1d0)), dimension(4) :: summom
  real(kind(1d0)) :: wt
  integer :: i,j,njets,njets_fj
  integer, dimension(nEcut) :: njets2
!
  real(kind(1d0)), dimension(100) :: Yn
  integer, dimension(nEcut,100) :: jetnum
  character(len=2) :: tmp_c
  character(len=20) :: histo,transition
  real(kind(1d0)), dimension(npart) :: yijs

! 
  real(kind(1d0)) :: t_start_direct, t_finish_direct, t_sum_direct
  real(kind(1d0)) :: t_start_fj, t_finish_fj, t_sum_fj
  real(kind(1d0)) :: t_start_trans, t_finish_trans, t_sum_trans

  real(kind(1d0)) :: R
!init seed!!!
  iseed=12345
! iseed=303816
! iseed=33516
!
! histo = 'histo.dat'
! transition = 'transitions.dat'
! open(unit=101,file=histo,action="write",status="replace")
! open(unit=102,file=transition,action="write",status="replace")

!
  t_sum_direct = 0d0  
  t_sum_fj = 0d0  
  t_sum_trans= 0d0  

  do j=1,nps
!
!Nullifikal arrays
    masses=0d0
    momenta=0d0
!
!Generate PS point
    call rambo(npart,Q,masses,momenta,wt)
! 
! do i=1,npart
!   print*,i, momenta(:,i)
! end do
!
!Clustering
    print*,"**************** PS point #:",j,"****************"
!
!Init CPU time for naive direct clustering
    call cpu_time(t_start_direct)
!
    do i=1,nbins
      call cluster_gen_kt(npart,momenta,p,Ecut(ee),ycut_bins(i),njets)
!     print*,"ycut:",ycut_bins(i),"number of jets:",njets
!     write(101,*) ycut_bins(i),njets
    end do
!
!Compute time for direct clustering
    call cpu_time(t_finish_direct)
    print '("Time of naive direct clustering = ",f6.3," seconds.")',t_finish_direct - t_start_direct
    t_sum_direct = t_sum_direct + (t_finish_direct - t_start_direct)
!
!Init CPU time for FastJet direct clustering with
    call cpu_time(t_start_fj)
!
    do i=1,nbins
      R = acos(1 - ycut_bins(i))
      call fjcoreeegenkt(momenta,npart,R,p,njets_fj)
! To test identical clustering
!     call cluster_gen_kt(npart,momenta,p,Ecut(ee),ycut_bins(i),njets)
!     print*,"ycut:",ycut_bins(i),"FastJet: ",njets_fj," naive: ",njets
!     write(101,*) ycut_bins(i),njets_fj
    end do
!
!Compute time for direct clustering
    call cpu_time(t_finish_fj)
    print '("Time of FastJet direct clustering = ",f6.3," seconds.")',t_finish_fj - t_start_fj
    t_sum_fj = t_sum_fj + (t_finish_fj - t_start_fj)
!
!Init CPU time for transition method clustering
    call cpu_time(t_start_trans)
!
    call calc_gen_kt_yij(npart,momenta,p,nEcut,Ecut,ycut_bins(1),Yn,jetnum)
!
!Compute time for transition method
    call cpu_time(t_finish_trans)
    print '("Time of finding transition values = ",f6.3," seconds.")',t_finish_trans - t_start_trans
    t_sum_trans = t_sum_trans + (t_finish_trans - t_start_trans)
!
!Print transition values
!   do i=1,size(Yn)
!    print*,i,"Yn:",Yn(i),"number of jets:",jetnum(ee,i)
!This for nice plot compatibility with GLE
!    if (jetnum(ee,i).ne.jetnum(ee,i+1)) then 
!      write(102,*) Yn(i),jetnum(ee,i)
!      write(102,*) Yn(i),jetnum(ee,i+1)
!    else
!      write(102,*) Yn(i),jetnum(ee,i)
!    end if
!    if (Yn(i).eq.0) exit
!   end do
!
  end do
!
!Close files    
! close(101)
! close(102)   
!
  print '("Total time of naive direct clustering = ",f10.3," seconds.")',t_sum_direct
  print '("Total time of FastJet direct clustering = ",f10.3," seconds.")',t_sum_fj
  print '("Total time of computing transition values = ",f10.3," seconds.")',t_sum_trans

end program main
!
!
!****GENERAL INCLUSIVE KT ALGORITHMS****
!
subroutine cluster_gen_kt(ntracks,ptracks,p,Emin,ycut,njets)
implicit none
!
! I/O
  integer, intent(in) :: ntracks
  real(kind(1d0)), dimension(4,ntracks), intent(in) :: ptracks
  real(kind(1d0)), intent(in) :: p
  real(kind(1d0)), intent(in) :: Emin
  real(kind(1d0)), intent(in) :: ycut
  integer, intent(out) :: njets
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
!
!First compute the measures and find the minimum
    do i=1,ntracks
      if (jetvec(i).ne.0) cycle
      Ei = pjet(4,i)
      diB = Ei**(2*p)
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
        dij = min(Ei**(2*p),Ej**(2*p))*(1 - costhetaij)/ycut
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
    else if(dij_min.gt.diB_min) then
!Jet i become an inclusive jet!
      jetvec(iBtrk) = 1
    end if
!Count jets
    npseudo = 0
    do i=1,ntracks
      if(jetvec(i).eq.0) npseudo = npseudo +1
    end do
!We quit when only one pseudo jet left
    if (npseudo.eq.1) exit
  end do
!Clusterization done!
!
!Apply energy cut to get the actual number of jets
!  At this point every jetvec value is -1 or 1 and one is left with 0
!  jets with jetvec -1 are always below the energy cut
!  since they are nullvectors
! We loop over multiple energy cuts
  njets = 0
  do i=1,ntracks
!   if (jetvec(i).eq.-1) cycle
    if(pjet(4,i).gt.Emin) then
      njets = njets + 1
    end if
  end do
!
end subroutine cluster_gen_kt
!
subroutine calc_gen_kt_yij(ntracks,ptracks,p,nEmin,Emin,ymin,Yn,jetnum)
implicit none
!
! I/O
  integer, intent(in) :: ntracks
  real(kind(1d0)), dimension(4,ntracks), intent(in) :: ptracks
  real(kind(1d0)), intent(in) :: p
  integer, intent(in) :: nEmin
  real(kind(1d0)), dimension(nEmin), intent(in) :: Emin
  real(kind(1d0)), intent(in) :: ymin
  real(kind(1d0)), dimension(100), intent(out) :: Yn
  integer, dimension(nEmin,100), intent(out) :: jetnum !limitation to 5 jets
!internal variables
  integer :: i, j, k,l,itrk,jtrk,iBtrk,njet,npseudo,resolved
  real(kind(1d0)) :: dij, diB, dij_min, diB_min, yij, yij_max, ycut
  real(kind(1d0)) :: Ei, Ej, piabs, pjabs, costhetaij
  real(kind(1d0)), dimension(4,ntracks) :: pjet 
  integer, dimension(ntracks) :: jetvec
  real(kind(1d0)), dimension(4) :: tmp_p
!
  Yn = 0
  jetnum = 0
!
  k = 1
!Set yij_max to 2
  yij_max=1
!
! We need ntracks-1 transition variables
  njet = 0
  do while(.true.)
!Copy momenta
   pjet = ptracks
!Everyone is a pseudojet: 0
   jetvec = 0
!ycut is yij_max taken from the last iteration
   ycut = yij_max
!yij_max reset to its minimum
   yij_max = 0
!We terminate if:
! - ycut < ymin
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
     do i=1,ntracks
       if (jetvec(i).ne.0) cycle
       Ei = pjet(4,i)
       diB = Ei**(2*p)
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
         dij = min(Ei**(2*p),Ej**(2*p))*(1 - costhetaij)/ycut
!Find yij_max
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
!Find yij_max
     yij = dij_min*ycut/diB_min
!    yij = dij_min/diB_min
!    print*,"dij_min",dij_min,"diB_min",diB_min,"yij",yij
!    print*,yij
     if((yij.gt.yij_max).AND.(yij.lt.ycut)) then
       yij_max = yij
     end if
!
!Decide what to do:
!Combine particles
     if(dij_min.lt.diB_min) then
!      print*,"CLUSTER!"
       tmp_p = pjet(:,itrk) + pjet(:,jtrk)
       pjet(:,itrk) = tmp_p
!  particle jtrk doesnt play in the game anymore
       jetvec(jtrk) = -1
       pjet(:,jtrk) = 0
     else if(dij_min.gt.diB_min) then
!Jet i become an inclusive jet!
       jetvec(iBtrk) = 1
     end if
!Count pseudo jets
     npseudo = 0
     do i=1,ntracks
      if (jetvec(i).eq.0) then
       npseudo = npseudo + 1
      end if
     end do
! If the number of jets is 1, quit the loop
     if (npseudo.eq.1) exit 
   end do !end of clustering loop
!
!Count jet candidates, and resolved jets, with different energy cuts
   do l=1,nEmin
    njet = 0
    resolved = 0
    do i=1,ntracks
     if (jetvec(i).eq.-1) cycle
     njet = njet + 1
     if (pjet(4,i).gt.Emin(l)) then
       resolved = resolved + 1
     end if
    end do
!Store number of resolved jets
    jetnum(l,k) = resolved
   end do
!Pass yij
   Yn(k) = yij_max
!  jetnum(k) = resolved
   k = k + 1
!
!We decrease ycut for the next iteration with tiny to avoid
!numerical artifacts
   yij_max = yij_max - 1d-10
!
  end do
!Clusterization done!
!
end subroutine calc_gen_kt_yij

