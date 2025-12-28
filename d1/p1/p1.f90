program d1
    use ord
    ! importante pra dizer q as vars vão ser todas declaradas explicitamente
    implicit none 
    
    integer :: tam                          ! tamanho da entrada
    integer, allocatable :: g1(:), g2(:)    ! vetores das duas listas
    
    integer :: input, stat                  ! variaveis controle p/ abrir os arq
    character(len=512) :: msg
    
    integer :: i                            ! iterador
    integer :: dist                         ! distancia (resposta)
    
    print *, 'Insira o tamanho da lista.'
    read (*,*) tam
    
    ! alocacao dos vetores
    allocate (g1(tam))
    allocate (g2(tam))
    
    ! abrindo a lista
    open(newunit=input, file="../input", status="old", action="read", iostat=stat, iomsg=msg)
    if (stat /= 0) then
        print *, trim(msg)
        error stop
    end if
    
    ! lendo input
    do i = 1, tam
        read(input, *) g1(i), g2(i)
    end do
    
    close(input)
    
    ! ordenando os vetores
    call mergeSort(g1, 1, tam)
    call mergeSort(g2, 1, tam)
    
    ! calcula a distancia
    dist = 0
    do i = 1, tam
       dist = dist + abs((g1(i) - g2(i)))
    end do
    
    print *, dist
    
    ! desalocacao dos vetores
    deallocate (g1)
    deallocate (g2)
    
end program
