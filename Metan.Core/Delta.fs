namespace Metan.Core

module ChangeDetection =
    type GameChange =
        | VehicleAdded of Vehicle
        | VehicleRemoved of VehicleId
        | VehicleMoved of VehicleId * HitBox * HitBox
        | VehicleShaped of VehicleId * Reflection * Reflection
        | VehicleDamaged of VehicleId * Health
        | VehicleHealed of VehicleId * Health
        | CrateAdded of Crate
        | CrateRemoved of CrateId
        | BulletAdded of Bullet
        | BulletRemoved of BulletId
        | BulletMoved of BulletId * HitBox * HitBox
    
    let delta (previous:Game) (current:Game) =
        let deltaVehicleHitBox (pv:Vehicle) (cv:Vehicle) =
            let ptl = HitBox.topLeft pv.hitBox
            let ctl = HitBox.topLeft cv.hitBox
            if ptl = ctl then []
            else [ VehicleMoved(pv.id, pv.hitBox, cv.hitBox) ]
        let deltaVehicleShape (pv:Vehicle) (cv:Vehicle) =
            match pv, cv with
            | { Vehicle.shape = Reflection ps }, { Vehicle.shape = Reflection cs } ->
                let pl = List.length ps
                let cl = List.length cs
                if pl = cl then []
                else [ VehicleShaped(pv.id, pv.shape, cv.shape) ]
        let deltaVehicleHealth (pv:Vehicle) (cv:Vehicle) =
            let ph = Vehicle.health pv
            let ch = Vehicle.health cv
            if ph = ch then []
            elif ph < ch then [ VehicleHealed (pv.id, ch) ]
            else [ VehicleDamaged (pv.id, ch) ]
        let deltaBulletHitBox (pb:Bullet) (cb:Bullet) =
            let ptl = HitBox.topLeft pb.hitBox
            let ctl = HitBox.topLeft cb.hitBox
            if ptl = ctl then []
            else [ BulletMoved(pb.id, pb.hitBox, cb.hitBox) ]
        let rec deltaVehicles pvs cvs acc =
            match pvs, cvs with
            | pv:Vehicle::ps, cv:Vehicle::cs ->
                if pv.id = cv.id
                then let changes =
                        List.concat [
                            deltaVehicleHitBox pv cv
                            deltaVehicleShape pv cv
                            deltaVehicleHealth pv cv
                            acc
                        ]
                     deltaVehicles ps cs changes
                elif pv.id > cv.id
                then deltaVehicles ps cvs (VehicleRemoved(pv.id)::acc)
                else deltaVehicles pvs cs (VehicleAdded(cv)::acc)
            | [], cv::cs -> deltaVehicles [] cs (VehicleAdded(cv)::acc)
            | pv::ps, [] -> deltaVehicles ps [] (VehicleRemoved(pv.id)::acc)
            | [], [] -> acc
        let rec deltaBullets pbs cbs acc =
            match pbs, cbs with
            | pb:Bullet::ps, cb:Bullet::cs ->
                if pb.id = cb.id
                then let changes =
                        List.concat [
                            deltaBulletHitBox pb cb
                            acc
                        ]
                     deltaBullets ps cs changes
                elif pb.id < cb.id
                then deltaBullets ps cbs (BulletRemoved(pb.id)::acc)
                else deltaBullets pbs cs (BulletAdded(cb)::acc)
            | [], cb::cs -> deltaBullets [] cs (BulletAdded(cb)::acc)
            | pb::cs, [] -> deltaBullets cs [] (BulletRemoved(pb.id)::acc)
            | [], [] -> acc
        let rec deltaCrates pcs ccs acc =
            match pcs, ccs with
             | pc:Crate::ps, cc:Crate::cs ->
                 if pc.id = cc.id
                 then deltaCrates ps cs acc
                 elif pc.id > cc.id
                 then deltaCrates ps ccs (CrateRemoved(pc.id)::acc)
                 else deltaCrates pcs cs (CrateAdded(cc)::acc)
             | [], cc::cs -> deltaCrates [] cs (CrateAdded(cc)::acc)
             | pc::cs, [] -> deltaCrates cs [] (CrateRemoved(pc.id)::acc)
             | [], [] -> acc
        [] |> deltaVehicles previous.vehicles current.vehicles
           |> deltaBullets previous.bullets current.bullets
           |> deltaCrates previous.crates current.crates

    let init (current:Game) =
        let vehicles =
            [ for vehicle in current.vehicles do
                VehicleAdded vehicle ]
        let crates =
            [ for crate in current.crates do
                CrateAdded crate ]
        let bullets =
            [ for bullet in current.bullets do
                BulletAdded bullet ]
        vehicles @ crates @ bullets