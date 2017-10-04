package com.dotgoing.prac

import javax.inject.Inject

import play.api.inject.ApplicationLifecycle

import scala.concurrent.Future

/**
  * Author: Sean
  * Date: 4/10/2017
  * Time: 2:59 PM
  */
class MyComponentImpl @Inject()(lifecycle: ApplicationLifecycle) extends MyComponent {
  // previous contents of Plugin.onStart
  lifecycle.addStopHook { () =>
    // previous contents of Plugin.onStop
    println("this -s : MyComponentImpl ")
    Future.successful(())
  }
}
