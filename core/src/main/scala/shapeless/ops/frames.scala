/*
 * Copyright (c) 2013 Arseniy Zhizhelev
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package shapeless
package ops

package frame {
	import shapeless.frame._
	/**
	 * Ordinary operations on Frames. 
	 * 
	 * @author Arseniy Zhizhelev
	 */
	final class FrameOps[TFrame<:Frame](frame:TFrame) {
		def hasSlot[SlotT2 <: Slot]
			(slotId: SlotT2)
			(implicit ev: TFrame#AllSlots <:< SlotT2 = null): Boolean =
				ev != null
	
		def slots:List[Slot] = {
			def slots0(frame:Frame, result:List[Slot]):List[Slot] = frame match {
				case EmptyFrame => result.reverse
				case FrameKWithSlot(slotId, innerFrame) =>
					slots0(innerFrame, slotId::result)
			}
			slots0(frame, Nil)
		}
	
		def initialValue:FrameInstance[TFrame] =
			new FrameInstanceC[TFrame](slots.map(slotId => slotId -> slotId.initialValue).toMap, frame)
	
		def getSlotValue[SlotT2 <: Slot]
			(map: Map[Slot, Any], slotId: SlotT2)
			(implicit ev: TFrame#AllSlots <:< SlotT2)
				: slotId.Type =
					map(slotId).asInstanceOf[slotId.Type]
	
		def toTyped[TInstance<:WithFrameInstance]
			(typeConstructor: FrameInstance[TInstance#TFrame] => TInstance)
				:TypedFrame[TInstance] = //, TFrame2>:this.type
					FrameWithSlotsAndType[TInstance](frame.asInstanceOf[TInstance#TFrame], typeConstructor)
	}
	
	class SlotC[T](initialValue0: T) extends Slot {
		type Type = T
		override def initialValue: Type = initialValue0
	}
	
	/** FrameInstance contains map with slot->value pairs.
	 *  It has compile-time protection from accessing slots that are absent in the frame.*/
	private
	class FrameInstanceC[+TFrame <: Frame] (map: Map[Slot, Any], val frame: TFrame) extends FrameInstance[TFrame] {
	
		require(map.keySet == frame.slots, s"The frame instance value $map has a different set of slots than required by frame $frame.")
		
		/** Apply has compile-time protection from accessing slots that are absent in the frame.*/
		def apply[SlotT2 <: Slot](slotId: SlotT2)(implicit ev: frame.AllSlots <:< SlotT2): slotId.type#Type =
			map(slotId).asInstanceOf[slotId.Type]
	
		def updated
			[SlotT2 <: Slot, T<:SlotT2#Type]
			(slotId: SlotT2, value:T)
			(implicit ev: frame.AllSlots <:< SlotT2)
				:FrameInstance[TFrame] =
					new FrameInstanceC[frame.type](map.updated(slotId, value), frame)
		def updatedWithUpdater
			[SlotT2 <: Slot, TIn>:SlotT2#Type, TOut<:SlotT2#Type]
			(slotId: SlotT2)(updater:TIn  => TOut)
			(implicit ev: frame.AllSlots <:< SlotT2) =
				new FrameInstanceC(map.updated(slotId, updater(apply(slotId))), frame)
	}

	class WithFrameInstanceOps[TInstance<:WithFrameInstance]
		(instance:TInstance, tf:TypedFrame[TInstance])
	{
		def apply[SlotT2 <: Slot](slotId: SlotT2)(implicit ev: TInstance#TFrame#AllSlots <:< SlotT2): SlotT2#Type =
			instance.fi.apply(slotId)

		def updated
			[SlotT2 <: Slot, T<:SlotT2#Type]
			(slotId: SlotT2, value: T)
		  (implicit ev:TInstance#TFrame#AllSlots <:<SlotT2):TInstance = {
			val updatedV = (instance:TInstance).fi.updated(slotId, value)
			tf.construct(updatedV)
		}
		def updatedWithUpdater
			[SlotT2 <: Slot, TIn>:SlotT2#Type, TOut<:SlotT2#Type]
			(slotId: SlotT2)(updater:TIn  => TOut)
			(implicit ev: TInstance#TFrame#AllSlots <:< SlotT2):TInstance  = {
			val updatedV = (instance:TInstance).fi.updatedWithUpdater(slotId)(updater)
			tf.construct(updatedV)
		}

		/** returns an object that represents a setter over the original object */
		def >>[SlotT2 <: Slot]
			(slotId: SlotT2)(implicit ev: TInstance#TFrame#AllSlots <:< SlotT2):FrameLens[TInstance, SlotT2#Type] = 
			FrameLens(instance.fi.apply(slotId), {(v:SlotT2#Type) => updated(slotId, v) })
	}
	
	class FrameLensOps[O, V](f:FrameLens[O, V]){
		/** returns an object that represents a setter over the original object */
		def >>[SlotT2 <: Slot, TInstance<:WithFrameInstance]
			(slotId: SlotT2)(implicit ev: V=:=TInstance, ev1:TInstance=:=V, ev2:TInstance#TFrame#AllSlots <:< SlotT2,  tf:TypedFrame[TInstance]):FrameLens[O, SlotT2#Type] = 
			FrameLens(f.get.asInstanceOf[TInstance].fi.apply(slotId), 
					{(v:SlotT2#Type) =>
						val instance = f.get:TInstance
						f.set(
								new WithFrameInstanceOps(instance, tf).updated(slotId, v):V
						) 
				})
		
	}
	
}
