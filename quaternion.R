# axis vector p = c(p1, p2, p3)
library(testit) # for assert
library(plotly)

mag = function(v){
    sqrt(sum(v^2))
}
unitVec = function(v){
    v/sqrt(sum(v^2))
}

makeQuaternion = function(p, theta){
    p = unitVec(p)
    c(cos(theta/2), sin(theta/2)*p)
}

q.inv = function(q){
    c(q[1], -q[2:4])
}

qproduct = function(q, p){ # order matters
    r = rep(0,4)
    r[1] = q[1]*p[1] - sum(q[2:4]*p[2:4])
    r[2] = q[1]*p[2] + q[2]*p[1] + q[3]*p[4] - q[4]*p[3]
    r[3] = q[1]*p[3] - q[2]*p[4] + q[3]*p[1] + q[4]*p[2]
    r[4] = q[1]*p[4] + q[2]*p[3] - q[3]*p[2] + q[4]*p[1]
    r
}

vec2quat = function(vec){
    c(0,vec)
}

rotate = function(vec, axis, theta){
    assert("vec: 3d vector required", length(vec)==3)
    assert("axis: 3d vector required", length(axis)==3)
    p = unitVec(axis)
    q = makeQuaternion(p, theta)
    v = vec
    v_new = qproduct(q,vec2quat(v))
    v_new = qproduct(v_new, q.inv(q))
    v_new[-1]
}

rotatebyQuat = function(vec, q){
    assert("vec: 3d vector required", length(vec)==3)
    assert("quaternion: 4d vector required", length(q)==4)
    v = vec
    v_new = qproduct(q,vec2quat(v))
    v_new = qproduct(v_new, q.inv(q))
    v_new[-1]
}

newFrame = function(q){
    Rw = diag(3)
    R = Rw
    for(i in 1:3)
        R[,i] = rotatebyQuat(Rw[,i], q)
    R
}

netQuaternion = function(init_q, operation, angle, bodyFrameRotation = T){
    #if(bodyFrameRotation){
    #    angle = rev(angle)
    #    operation = rev(operation)
    #}
    q0 = init_q
    Rw = diag(3)
    for(k in 1:length(angle)){
        i = switch(operation[k], yaw = 3, pitch = 2, roll = 1)
        q = makeQuaternion(Rw[,i],angle[k])
        if(bodyFrameRotation){
            q0 = qproduct(q0,q)
        } else{
            q0 = qproduct(q,q0)
        }
    }
    q0
}

if(F){
rotateFrame = function(init_frame, operation, angle, bodyFrameRotations = T){
    R = init_frame
    Rw = diag(3)
    for(k in 1:length(angle)){
        i = switch(operation[k], yaw = 3, pitch = 2, roll = 1)
        assert("wrong operation: use 'yaw' 'pitch' or 'roll' only", !is.null(i))
        if(bodyFrameRotations){
            for(j in 1:3){
                if(i!=j)
                    R[,j] = rotate(R[,j], R[,i], angle[k])
            }
        } else{
            for(j in 1:3)
                R[,j] = rotate(R[,j], Rw[,i], angle[k])
        }
    }
    R
}


rotateFrame_deprecated = function(init_frame, yaw, pitch, roll, bodyFrameRotations = T){
    angle = c(yaw, pitch, roll)
    R = init_frame
    # order: 
    # 1. yaw (around body z axis)
    # 2. pitch (around body y axis)
    # 3. roll (around body x axis)
    if(bodyFrameRotations){
        for(i in 3:1){ 
            for(j in 1:3){
                if(i!=j)
                    R[,j] = rotate(R[,j], R[,i], angle[4-i])
            }
        }
    } else{
        Rw = diag(3)
        for(i in 3:1){
            for(j in 1:3){
                R[,j] = rotate(R[,j], Rw[,i], angle[4-i])
            }
        }
    }
    R
}

rotationMatrix = function(phi, theta, psi){
    R = matrix(0,3,3)
    cphi = cos(phi); sphi = sin(phi)
    ct = cos(theta); st = sin(theta)
    csi = cos(psi); ssi = sin(psi)
    
    R[1,] = c(ct*cphi, ssi*st*cphi - csi*sphi, 
              csi*st*cphi + ssi*sphi)
    R[2,] = c(ct*sphi, ssi*st*sphi + csi*cphi, 
              csi*st*sphi - ssi*cphi)
    R[3,] = c(-st, ssi*ct, csi*ct)
    R
}
}

R = diag(3)
qnet = c(1,0,0,0)

for(i in 1:10){
    n = sample(1:20,1)
    op = sample(dict,n,T)
    ang = runif(n,-pi,pi)
    body = sample(c(T,F),1)
    R = rotateFrame(R, op, ang, body)
    qnet = netQuaternion(qnet,op,ang, body)
}
R2 = newFrame(qnet)

a=0.9
b=1
pos = matrix(0,100,3)
vel = matrix(0,100,3)
for(i in 1:99){
    pos[i+1,]  = pos[i,] + vel[i,]
    vel[i+1,] = a*vel[i,] + b*rnorm(3)
}

getOrientation = function(heading_vec, default_heading = T){
    if(default_heading)
        uw = c(0,1,0)
    uh = unitVec(heading_vec)
    q = qproduct(vec2quat(uw), vec2quat(uh))
    u = unitVec(q[2:4])
    th = atan2(mag(q[2:4]), -q[1])
    qnew = makeQuaternion(u, th)
    R_ = newFrame(qnew)
    R_
}

p <- plot_ly(x = pos[,1], y = pos[,2], z = pos[,3], type = 'scatter3d', mode = 'lines',
             opacity = 50, line = list(width = 2, color = 1, reverscale = FALSE)) %>% 
    add_trace()
p

#chart_link = api_create(p, filename="line3d/basic")
#chart_link

