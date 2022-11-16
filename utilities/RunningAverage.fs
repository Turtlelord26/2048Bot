namespace Average

type RunningAverage = {total: double; count: int}
    with

    static member Zero = {total = 0.; count = 0}

    static member inline (+) (ra1, ra2) = 
        {total = ra1.total + ra2.total; count = ra1.count + ra2.count}
    
    static member construct value =
        {total = value; count = 1}
    
    static member consume runningAverage =
        runningAverage.total / double runningAverage.count
