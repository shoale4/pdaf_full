!!!! NEW SB 1/23/24 !!!!
!! holds functions that read and write to binary files !!

module output_bin

    implicit none
    save
    public

	contains

		subroutine write_txt(dim, state, filename)

			implicit none

			! args
			integer, intent(in) :: dim
			real(8), intent(in) :: state(dim)
			character(len=32), intent(in) :: filename

			! local vars
			integer :: nx

			OPEN (11, file='txt_file_data/state_step_'//TRIM(filename), status='replace', form='unformatted', & 
                  access='stream', action='write')
			write (11) state
			CLOSE(11)

		end subroutine

		subroutine read_txt(filename, state_p)

			implicit none

			! args
			character(len=16), intent(in) :: filename

			! local vars
			real(8), intent(out) :: state_p(200*200)

			OPEN (11, file='txt_file_data/ens_obs/'//TRIM(filename), form='unformatted', access='stream', action='read')
			read (11) state_p
			CLOSE(11)

		end subroutine

end module output_bin