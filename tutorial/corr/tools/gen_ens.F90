!! sb new 1/23/24

program gen_ens

	use output_bin, &
		only: write_txt, read_txt

	use parser_no_mpi, &
		only: parse

	implicit none


	! local vars
	! some of this stuff is hardcoded bc the txt file stuff is really just to get this to run
	character(len=32) :: handle
	character(len=3) :: cnt_str
	character(len=16) :: filename
	integer :: ens_size
	integer :: nsteps
	integer :: iter, add_noise, cnt
	real(8), allocatable :: noise(:)
	real(8), allocatable :: state(:)
	integer :: nx
	integer :: iseed(4)   ! for random number generation


	handle = 'ens_size'
  	call parse(handle, ens_size) 
  	nsteps = 18000
  	nx = 200
  	allocate(state(nx*nx))
  	allocate(noise(nx*nx))

	write (*,*) "Start ensemble generation"
  	write (*,*) nsteps

  	cnt = 1
  	do iter = 5, nsteps, (nsteps)/(ens_size)
 		! read state vector	
		write(filename, "(I0)") iter
		call read_txt(trim(filename), state)

		! create noise (new sb 3/30/23)
		add_noise = 1

		if (add_noise == 0) then
			noise(:) = 0
		else
			iseed(1) = 543
			iseed(2) = 98
			iseed(3) = 1003
			iseed(4) = 31
			call dlarnv(3, iseed, nx*nx, noise)
			noise = noise / maxval(noise)
			! print *, noise
		end if

		state = state + noise

        write(*,*) "Writing ensemble member: ", cnt
		write(cnt_str, "(I0)") cnt
		call write_txt(nx*nx, reshape(state, (/nx*nx/)), 'ens_'//TRIM(cnt_str))

		cnt = cnt+1


	end do 





end program