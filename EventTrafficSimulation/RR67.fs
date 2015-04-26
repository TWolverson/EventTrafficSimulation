module RR67

[<Measure>] type pcu
[<Measure>] type h
[<Measure>] type flow = pcu/h
[<Measure>] type flowratio = flow^-1 // not sure if this is needed/right
[<Measure>] type m

let f = 4.0 // proportion of right-turners
let S0 = 1234.0<flow>
let r = 4.0<m> // turn radius
let q0 = 4.5<flow>
let lm = 0.5 // proportion of cycle which is effective green; <.0
let nl = 2 //number of opposing lanes
let so = 455.<flow> //saturation flow per lane of opposing entry
let X0 = q0 / (lm * float nl * so)
let Ns = 1234.6<pcu>
let t1 = 12.0 * (X0 ** 2.0) / (1.0 + 0.6 * (1.0 - f) * Ns)
let t2 = 1.0 - (f * (X0 ** 2.0))
let T =  1.0 + 1.5<m> / r + t1 / t2



let Sg = (S0 - 230.0<flow>) / (1.0 + (T - 1.0) * f)

type EntryGeometry = {
    NumLanes : int;
    SatFlowPerLane : float<flow>;
    StorageSpace : float<pcu> option
}

type JunctionGeometry = {
    Measured : EntryGeometry;
    Opposing : EntryGeometry option;
}    

type Stage = {
    StartTime : int;
    EndTime : int;
}

type SignalSettings = {
    Stages : Stage list
}

type Model = {
    Geometry : JunctionGeometry;
    Signals : SignalSettings option;
}    

let OpposedCase f lm nl so = 
    let X0 = q0 / (lm * float nl * so)
    let Ns = 1234.6
    let t1 = 12.0 * (X0 ** 2.0) / (1.0 + 0.6 * (1.0 - f) * Ns)
    let t2 = 1.0 - (f * (X0 ** 2.0))
    let T =  1.0 + 1.5 / r + t1 / t2
    (S0 - 230.0) / (1.0 + (T - 1.0) * f)

let DefaultOpposing f lm = OpposedCase f lm 1 1800.0