import numpy as np

def AngVel_BodytoInertial(phi, theta, psi):
    """
        Tranform angular velocities from Body frame of reference
        to inertial frame of reference
    """

    """
        Phi = Roll (rotation around x axis)
        Theta = Pitch (rotation around y axis)
        Psi = Yaw (rotation around z axis)
    """
    D = np.eye(3)
    D[:,1] = [np.sin(phi)*np.tan(theta), np.cos(phi), np.sin(phi)/np.cos(theta)]
    D[:,2] = [np.cos(phi)*np.tan(theta), - np.sin(phi), np.cos(phi)/np.cos(theta)]
    return D

def Tranform_BodytoInertial(phi, theta, psi):
    """
        Transform vector from Body frame to inertial frame of reference
    """

    """
        Phi = Roll (rotation around x axis)
        Theta = Pitch (rotation around y axis)
        Psi = Yaw (rotation around z axis)
    """
    cPsi = np.cos(psi)
    sPsi = np.sin(psi)
    cPhi = np.cos(phi)
    sPhi = np.sin(phi)
    cTheta = np.cos(theta)
    sTheta = np.sin(theta)
    R = np.eye(3)
    R[:,0] = [cPsi*cTheta, cTheta*sPsi, -sTheta]
    R[:,1] = [cPsi*sPhi*sTheta - cPhi*sPsi, cPhi*cPsi + sPhi*sPsi*sTheta, cTheta*sPhi]
    R[:,2] = [sPhi*sPsi + cPhi*cPsi*sTheta, cPhi*sPsi*sTheta - cPsi*sPhi, cPhi*cTheta]
    return R


alpha = 0.0 # angular acceleration term

angVel = -tau*angVel + noise
angVelFix = Tranform_BodytoInertial(phi,theta,psi).dot(angVel)

EulerAngles = EulerAngles + angVelFix

vel = -tau*vel + noise
position = position + vel
